{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Glob ( globTool ) where

import           Relude


import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import           Control.Lens           ( (?~) )
import           System.Directory     ( doesDirectoryExist )
import           System.FilePath.Glob ( compile, globDir1 )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext, ToolExecutorType(..) )

globDescription :: Text
globDescription = """
Fast file pattern matching tool. Returns matching file paths sorted by modification time.

Usage:
- Supports glob patterns: **/*.hs, src/**/*.ts, *.txt
- Use ** for recursive directory matching
- Use * for single directory level matching
- Results limited to 100 files

When to use:
- Finding files by name pattern
- Locating specific file types in a directory tree
- Use this instead of `find` or `ls` commands in bash
"""

globTool :: BuiltinTool
globTool = BuiltinTool
  { _btTool = makeTool "glob" inputSchema
      & toolDescription ?~ globDescription
  , _btExecutor = SimpleExecutor executeGlob
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "pattern" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Glob pattern (e.g., '**/*.hs', 'src/*.txt')" :: Text)
              ]
          , "path" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Base directory to search in (default: current directory)" :: Text)
              ]
          ]
      , "required" Aeson..= (["pattern"] :: [Text])
      ]

executeGlob :: ToolContext -> Aeson.Value -> IO ToolResult
executeGlob _ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (globPattern, basePath) -> do
      exists <- doesDirectoryExist (toString basePath)
      if not exists
        then pure $ ToolResult False ("Directory not found: " <> basePath)
        else do
          let compiledPattern = compile (toString globPattern)
          matches <- globDir1 compiledPattern (toString basePath)
          let limited = take 100 matches
          if null matches
            then pure $ ToolResult True "No files found matching pattern."
            else do
              let output = T.unlines (map toText limited)
                  suffix = if length matches > 100
                    then "\n... (showing 100 of " <> T.pack (show (length matches)) <> " matches)"
                    else ""
              pure $ ToolResult True (output <> suffix)
  where
    parseArgs = Aeson.withObject "GlobArgs" $ \o -> do
      globPattern <- o .: "pattern"
      basePath <- (o .:? "path") <&> fromMaybe "."
      pure (globPattern :: Text, basePath :: Text)
