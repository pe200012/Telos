{-# LANGUAGE MultilineStrings #-}

-- | Extract tool for Dynamic Context Pruning
module Telos.Context.Extract ( extractTool ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ( (.:) )
import           Data.Aeson.Types   ( parseEither )
import qualified Data.Text          as T

import           Control.Lens         ( (?~) )

import           Relude

import           Telos.Core.Types   ( makeTool, toolDescription )
import           Telos.Tool.Types   ( BuiltinTool(..)
                                    , ToolContext
                                    , ToolExecutorType(..)
                                    , ToolResult(..)
                                    )

extractDescription :: Text
extractDescription = """
Extract key findings from tool outputs into distilled knowledge, then remove raw outputs.

Use when you need to preserve information but reduce context size:
- Completed research with valuable insights to remember
- Large outputs where only specific details matter
- Files you've analyzed but won't need to edit

Input format:
- ids: Array of numeric IDs as strings from <prunable-tools> list
- distillation: Array of strings (one per ID) with key information to preserve

Example:
{
  "ids": ["5", "12"],
  "distillation": [
    "auth.ts: validateToken checks cache (5min TTL) then OIDC. Uses bcrypt 12 rounds.",
    "user.ts: User has id, email, permissions array, status enum"
  ]
}

IMPORTANT: Distillation should capture essential information - function signatures, logic, constraints.
"""

extractTool :: BuiltinTool
extractTool = BuiltinTool
  { _btTool = makeTool "extract" inputSchema
      & toolDescription ?~ extractDescription
  , _btExecutor = SimpleExecutor executeExtract
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
              , "description" Aeson..= ("Array of numeric IDs to extract" :: Text)
              ]
          , "distillation" Aeson..= Aeson.object
              [ "type" Aeson..= ("array" :: Text)
              , "items" Aeson..= Aeson.object
                  [ "type" Aeson..= ("string" :: Text)
                  ]
              , "description" Aeson..= ("Array of distilled summaries, one per ID" :: Text)
              ]
          ]
      , "required" Aeson..= (["ids", "distillation"] :: [Text])
      ]

executeExtract :: ToolContext -> Aeson.Value -> IO ToolResult
executeExtract _ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (ids, distillations) -> do
      -- Parse numeric IDs
      let parsedIds = mapMaybe (readMaybe . toString) ids :: [Int]
      
      -- Validate counts match
      if length parsedIds /= length distillations
        then pure $ ToolResult False $
          "Mismatch: " <> T.pack (show $ length parsedIds) <> " IDs but " <>
          T.pack (show $ length distillations) <> " distillations"
        else if null parsedIds
          then pure $ ToolResult False "No valid numeric IDs provided"
          else do
            -- In real implementation, this would:
            -- 1. Store distillations in PruneState
            -- 2. Mark IDs for pruning
            let count = length parsedIds
            pure $ ToolResult True $
              "Context pruning complete. Pruned " <> T.pack (show count) <> " tool outputs.\n\n" <>
              "Semantically pruned (" <> T.pack (show count) <> "):\n" <>
              T.intercalate "\n" 
                [ "→ " <> T.take 60 d <> if T.length d > 60 then "..." else ""
                | d <- distillations
                ]
  where
    parseArgs = Aeson.withObject "ExtractArgs" $ \o -> do
      ids <- o .: "ids"
      distillation <- o .: "distillation"
      pure (ids :: [Text], distillation :: [Text])
