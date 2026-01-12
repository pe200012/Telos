{-# LANGUAGE TemplateHaskell #-}

-- | Message transformation for Dynamic Context Pruning
module Telos.Context.Transform
  ( -- * Message Transformation
    transformMessages
  , transformMessage
    -- * Prunable List Generation
  , getPrunableInfos
  , formatPrunableList
  , injectPrunableList
    -- * Tool Cache Building
  , buildToolCache
  , updateToolCache
    -- * Token Estimation
  , estimateTokens
  , estimateMessageTokens
  , estimateContextTokens
    -- * Placeholders
  , outputRemovedPlaceholder
  , contentRemovedPlaceholder
  ) where

import qualified Data.Aeson          as Aeson
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T

import           Lens.Micro          ( (%~), (^.) )

import           Relude

import           Telos.Context.Types
import           Telos.Core.Types    ( Message(..)
                                     , amContent
                                     , amToolCalls
                                     , tcArguments
                                     , tcName
                                     )

-- | Placeholder text for removed output
outputRemovedPlaceholder :: Text
outputRemovedPlaceholder = "[Output removed to save context - information superseded or no longer needed]"

-- | Placeholder text for removed content (write/edit inputs)
contentRemovedPlaceholder :: Text
contentRemovedPlaceholder = "[content removed to save context, this is not what was written to the file, but a placeholder]"

-- | Transform messages before sending to LLM
-- Replaces pruned content with placeholders or distillations
transformMessages :: PruneState -> [Message] -> [Message]
transformMessages pstate = zipWith (transformWithId pstate) [0..]

transformWithId :: PruneState -> Int -> Message -> Message
transformWithId pstate idx msg = case msg of
  ToolResultMessage callId toolName _result isErr ->
    let rid = ToolResultId idx
    in if rid `Set.member` (pstate ^. psMarkedIds)
       then case Map.lookup rid (pstate ^. psDistillations) of
              Just distill -> ToolResultMessage callId toolName distill isErr
              Nothing -> ToolResultMessage callId toolName outputRemovedPlaceholder isErr
       else msg
  _ -> msg

-- | Transform a single message (when ID is known)
transformMessage :: PruneState -> ToolResultId -> Message -> Message
transformMessage pstate rid msg = case msg of
  ToolResultMessage callId toolName _result isErr ->
    if rid `Set.member` (pstate ^. psMarkedIds)
    then case Map.lookup rid (pstate ^. psDistillations) of
           Just distill -> ToolResultMessage callId toolName distill isErr
           Nothing -> ToolResultMessage callId toolName outputRemovedPlaceholder isErr
    else msg
  _ -> msg

-- | Get list of prunable tool results (not yet pruned, eligible for manual pruning)
getPrunableInfos :: PruneConfig -> PruneState -> [PrunableInfo]
getPrunableInfos config pstate =
  let cache = pstate ^. psToolCache
      marked = pstate ^. psMarkedIds
      currentTurn = pstate ^. psCurrentTurn
      protectedTurns = config ^. pcTurnProtection
      protectedTools = Set.fromList (config ^. pcProtectedTools)
      
  in [ PrunableInfo
       { _piId = rid
       , _piToolName = entry ^. tceToolName
       , _piParams = summarizeParams (entry ^. tceToolName) (entry ^. tceFilePath)
       , _piTokens = entry ^. tceTokenEstimate
       , _piTurnAge = currentTurn - entry ^. tceTurnIndex
       }
     | (rid, entry) <- Map.toList cache
     , not (rid `Set.member` marked)  -- Not already marked
     , (currentTurn - entry ^. tceTurnIndex) >= protectedTurns  -- Past protection window
      , not ((entry ^. tceToolName) `Set.member` protectedTools)  -- Not protected tool
     ]

-- | Summarize parameters for display
summarizeParams :: Text -> Maybe FilePath -> Text
summarizeParams toolName mFilePath = case mFilePath of
  Just fp -> toText fp
  Nothing -> toolName <> " output"

-- | Format prunable list for injection into conversation
formatPrunableList :: [PrunableInfo] -> Text
formatPrunableList infos
  | null infos = ""
  | otherwise = T.unlines $
      [ "<prunable-tools>"
      , "The following tool outputs can be pruned to save context."
      , "Use `discard` to remove or `extract` to distill before removing."
      , ""
      ] ++
      map formatInfo infos ++
      [ "</prunable-tools>" ]
  where
    formatInfo info = T.pack (show $ _unToolResultId $ info ^. piId)
      <> ": " <> info ^. piToolName
      <> ", " <> info ^. piParams

-- | Inject prunable list as a system message at the end
injectPrunableList :: PruneConfig -> PruneState -> [Message] -> [Message]
injectPrunableList config pstate msgs =
  let infos = getPrunableInfos config pstate
  in if null infos
     then msgs
     else msgs ++ [SystemMessage $ formatPrunableList infos]

-- | Build initial tool cache from message history
buildToolCache :: [Message] -> Map ToolResultId ToolCacheEntry
buildToolCache = snd . foldl' processMessage (0, Map.empty)
  where
    processMessage :: (Int, Map ToolResultId ToolCacheEntry) -> Message -> (Int, Map ToolResultId ToolCacheEntry)
    processMessage (turnIdx, cache) msg = case msg of
      UserMessage _ -> (turnIdx + 1, cache)  -- New turn on user message
      ToolResultMessage _callId toolName result isErr ->
        let rid = ToolResultId (Map.size cache)
            entry = ToolCacheEntry
              { _tceToolName = toolName
              , _tceParams = Aeson.Null  -- Would need to store params separately
              , _tceParamKey = toolName <> ":" <> T.take 8 result  -- Simplified
              , _tceTurnIndex = turnIdx
              , _tceIsError = isErr
              , _tceFilePath = Nothing  -- Would need to extract from params
              , _tceTokenEstimate = estimateTokens result
              }
        in (turnIdx, Map.insert rid entry cache)
      _ -> (turnIdx, cache)

-- | Update tool cache with a new tool result
updateToolCache :: PruneState -> Text -> Text -> Bool -> Maybe FilePath -> Text -> PruneState
updateToolCache pstate toolName paramKey isErr mFilePath result =
  let nextId = ToolResultId (Map.size $ pstate ^. psToolCache)
      entry = ToolCacheEntry
        { _tceToolName = toolName
        , _tceParams = Aeson.Null
        , _tceParamKey = paramKey
        , _tceTurnIndex = pstate ^. psCurrentTurn
        , _tceIsError = isErr
        , _tceFilePath = mFilePath
        , _tceTokenEstimate = estimateTokens result
        }
  in pstate & psToolCache %~ Map.insert nextId entry

-- | Rough token estimation (4 chars per token average)
estimateTokens :: Text -> Int
estimateTokens t = T.length t `div` 4

-- | Estimate tokens for a single message
estimateMessageTokens :: Message -> Int
estimateMessageTokens msg = case msg of
  SystemMessage content         -> estimateTokens content + 4  -- role overhead
  UserMessage content           -> estimateTokens content + 4
  AssistantMsg am               -> maybe 0 estimateTokens (am ^. amContent) + 4
    + sum [ estimateTokens (tc ^. tcName) + estimateTokens (encodeArgs $ tc ^. tcArguments)
          | tc <- am ^. amToolCalls
          ]
  ToolResultMessage _ name res _ -> estimateTokens name + estimateTokens res + 4
  where
    encodeArgs v = decodeUtf8 $ toStrict $ Aeson.encode v

-- | Estimate total context tokens, accounting for DCP pruning
-- Returns (rawTokens, effectiveTokens) where effectiveTokens considers pruned content
estimateContextTokens :: PruneState -> [Message] -> (Int, Int)
estimateContextTokens pstate msgs =
  let rawTokens = sum $ map estimateMessageTokens msgs
      -- Calculate tokens saved by pruning
      prunedTokens = sum [ entry ^. tceTokenEstimate
                        | (rid, entry) <- Map.toList (pstate ^. psToolCache)
                        , Set.member rid (pstate ^. psMarkedIds)
                        ]
      -- Add back distillation tokens
      distillationTokens = sum [ estimateTokens t
                               | t <- Map.elems (pstate ^. psDistillations)
                               ]
      effectiveTokens = rawTokens - prunedTokens + distillationTokens
  in (rawTokens, max 0 effectiveTokens)
