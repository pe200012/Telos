{-# LANGUAGE TemplateHaskell #-}

-- | Pruning strategies for Dynamic Context Pruning
module Telos.Context.Strategy
  ( -- * Strategy Type
    PruneStrategy
    -- * Built-in Strategies
  , deduplicationStrategy
  , supersedeWritesStrategy
  , purgeErrorsStrategy
    -- * Strategy Pipeline
  , runStrategies
    -- * Utilities
  , extractFilePath
  , computeParamKey
  ) where

import qualified Crypto.Hash.SHA256     as SHA256

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           Data.Aeson             ( Value(..), encode )

import           Control.Lens             ( (%~), (^.), (^?) )
import           Data.Aeson.Lens       ( key, _String )

import           Relude

import           Telos.Context.Types

-- | A pruning strategy takes current state and returns updated state with new marks
type PruneStrategy = PruneConfig -> PruneState -> PruneState

-- | Run all strategies in sequence
runStrategies :: PruneConfig -> PruneState -> PruneState
runStrategies config pstate
  | not (config ^. pcEnabled) = pstate
  | otherwise = foldl' (\s strategy -> strategy config s) pstate
      [ deduplicationStrategy
      , supersedeWritesStrategy
      , purgeErrorsStrategy
      ]

-- | Deduplication: mark older duplicates for pruning
-- Groups tool calls by (toolName, paramKey), keeps most recent
deduplicationStrategy :: PruneStrategy
deduplicationStrategy config pstate =
  let cache = pstate ^. psToolCache
      currentTurn = pstate ^. psCurrentTurn
      protectedTurns = config ^. pcTurnProtection
      protectedTools = Set.fromList (config ^. pcProtectedTools)
      
      -- Group entries by paramKey
      groups :: Map Text [(ToolResultId, ToolCacheEntry)]
      groups = Map.foldrWithKey groupByKey Map.empty cache
      
      groupByKey :: ToolResultId -> ToolCacheEntry -> Map Text [(ToolResultId, ToolCacheEntry)] -> Map Text [(ToolResultId, ToolCacheEntry)]
      groupByKey rid entry acc =
        let k = entry ^. tceParamKey
        in Map.insertWith (++) k [(rid, entry)] acc
      
      -- Find IDs to mark (all but most recent in each group)
      idsToMark :: Set ToolResultId
      idsToMark = Set.fromList $ concatMap findOlderDuplicates (Map.elems groups)
      
      findOlderDuplicates :: [(ToolResultId, ToolCacheEntry)] -> [ToolResultId]
      findOlderDuplicates entries
        | length entries <= 1 = []
        | otherwise =
            let sorted = sortOn (negate . (^. tceTurnIndex) . snd) entries  -- newest first
            in case sorted of
                 (_newest : older) -> mapMaybe (filterPrunable currentTurn protectedTurns protectedTools) older
                 [] -> []
      
      filterPrunable :: Int -> Int -> Set Text -> (ToolResultId, ToolCacheEntry) -> Maybe ToolResultId
      filterPrunable curTurn protection protected (rid, entry)
        | (curTurn - entry ^. tceTurnIndex) < protection = Nothing  -- Protected by turn
        | (entry ^. tceToolName) `Set.member` protected = Nothing     -- Protected tool
        | otherwise = Just rid
        
  in pstate & psMarkedIds %~ Set.union idsToMark

-- | Supersede writes: mark writes when file was subsequently read
-- If write/edit followed by read of same file, mark write for pruning
supersedeWritesStrategy :: PruneStrategy
supersedeWritesStrategy config pstate =
  let cache = pstate ^. psToolCache
      currentTurn = pstate ^. psCurrentTurn
      protectedTurns = config ^. pcTurnProtection
      
      -- Find all file operations grouped by path
      fileOps :: Map FilePath [(ToolResultId, ToolCacheEntry)]
      fileOps = Map.foldrWithKey groupByFile Map.empty cache
      
      groupByFile :: ToolResultId -> ToolCacheEntry -> Map FilePath [(ToolResultId, ToolCacheEntry)] -> Map FilePath [(ToolResultId, ToolCacheEntry)]
      groupByFile rid entry acc = case entry ^. tceFilePath of
        Nothing -> acc
        Just fp -> Map.insertWith (++) fp [(rid, entry)] acc
      
      -- Find write operations superseded by later reads
      idsToMark :: Set ToolResultId
      idsToMark = Set.fromList $ concatMap findSupersededWrites (Map.elems fileOps)
      
      findSupersededWrites :: [(ToolResultId, ToolCacheEntry)] -> [ToolResultId]
      findSupersededWrites entries =
        let sorted = sortOn ((^. tceTurnIndex) . snd) entries  -- oldest first
            -- Find the latest read operation
            readOps = filter (isReadOp . snd) sorted
            writes = filter (isWriteOp . snd) sorted
            latestReadTurn = case readOps of
              [] -> -1
              _  -> foldr max (-1) $ map ((^. tceTurnIndex) . snd) readOps
        in [ rid | (rid, entry) <- writes
                 , entry ^. tceTurnIndex < latestReadTurn  -- Write before read
                 , (currentTurn - entry ^. tceTurnIndex) >= protectedTurns
           ]
      
      isReadOp entry = entry ^. tceToolName == "read"
      isWriteOp entry = (entry ^. tceToolName) `elem` ["write", "edit"]
      
  in pstate & psMarkedIds %~ Set.union idsToMark

-- | Purge errors: mark error outputs older than threshold
purgeErrorsStrategy :: PruneStrategy
purgeErrorsStrategy config pstate =
  let cache = pstate ^. psToolCache
      currentTurn = pstate ^. psCurrentTurn
      threshold = config ^. pcErrorThreshold
      protectedTools = Set.fromList (config ^. pcProtectedTools)
      
      idsToMark :: Set ToolResultId
      idsToMark = Set.fromList
        [ rid
        | (rid, entry) <- Map.toList cache
        , entry ^. tceIsError
        , (currentTurn - entry ^. tceTurnIndex) >= threshold
        , not ((entry ^. tceToolName) `Set.member` protectedTools)
        ]
  in pstate & psMarkedIds %~ Set.union idsToMark

-- | Extract file path from tool parameters
extractFilePath :: Text -> Value -> Maybe FilePath
extractFilePath toolName params
  | toolName `elem` ["read", "write", "edit"] =
      toString <$> (params ^? key "filePath" . _String)
  | toolName == "bash" =
      toString <$> (params ^? key "workdir" . _String)
  | otherwise = Nothing

-- | Compute a unique key for deduplication (tool:param_hash)
computeParamKey :: Text -> Value -> Text
computeParamKey toolName params =
  let paramBytes = toStrict $ encode params
      hash = T.take 8 $ TE.decodeUtf8 $ Base16.encode $ SHA256.hash paramBytes
  in toolName <> ":" <> hash
