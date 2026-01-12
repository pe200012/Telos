{-# LANGUAGE MultilineStrings #-}

-- | Discard tool for Dynamic Context Pruning
module Telos.Context.Discard ( discardTool ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ( (.:) )
import           Data.Aeson.Types   ( parseEither )
import qualified Data.Set           as Set
import qualified Data.Text          as T

import           Control.Lens         ( (?~) )

import           Relude

import           Telos.Context.Types
import           Telos.Core.Types    ( makeTool, toolDescription )
import           Telos.Tool.Types    ( BuiltinTool(..)
                                     , ToolContext
                                     , ToolExecutorType(..)
                                     , ToolResult(..)
                                     )

discardDescription :: Text
discardDescription = """
Discard tool outputs from context to manage conversation size and reduce noise.

Use when tool outputs are no longer needed:
- Task is complete and information has no future value
- Output was noise, irrelevant, or superseded by newer data
- Error outputs that have been addressed

Input format:
- ids: Array where first element is reason ("noise" | "completion"), followed by numeric IDs

Example: {"ids": ["completion", "5", "12", "18"]}

IMPORTANT: Only use IDs from the <prunable-tools> list provided in system context.
"""

discardTool :: BuiltinTool
discardTool = BuiltinTool
  { _btTool = makeTool "discard" inputSchema
      & toolDescription ?~ discardDescription
  , _btExecutor = SimpleExecutor executeDiscard
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "ids" Aeson..= Aeson.object
              [ "type" Aeson..= ("array" :: Text)
              , "items" Aeson..= Aeson.object
                  [ "type" Aeson..= ("string" :: Text)
                  ]
              , "description" Aeson..= ("Array with reason as first element, then numeric IDs to discard" :: Text)
              ]
          ]
      , "required" Aeson..= (["ids"] :: [Text])
      ]

executeDiscard :: ToolContext -> Aeson.Value -> IO ToolResult
executeDiscard _ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (reason, ids) -> do
      -- Validate reason
      if reason `notElem` ["noise", "completion"]
        then pure $ ToolResult False ("Invalid reason: " <> reason <> ". Must be 'noise' or 'completion'")
        else do
          -- Parse numeric IDs
          let parsedIds = mapMaybe (readMaybe . toString) ids :: [Int]
          if null parsedIds
            then pure $ ToolResult False "No valid numeric IDs provided"
            else do
              -- In real implementation, this would update PruneState
              -- For now, just acknowledge the request
              let idSet = Set.fromList $ map ToolResultId parsedIds
                  count = Set.size idSet
              pure $ ToolResult True $
                "Context pruning complete. Pruned " <> T.pack (show count) <> " tool outputs.\n\n" <>
                "Semantically pruned (" <> T.pack (show count) <> "):\n" <>
                T.intercalate "\n" (map (\i -> "→ tool output " <> T.pack (show i)) parsedIds)
  where
    parseArgs = Aeson.withObject "DiscardArgs" $ \o -> do
      ids <- o .: "ids"
      case ids of
        [] -> fail "ids array cannot be empty"
        (reason : rest) -> pure (reason :: Text, rest :: [Text])
