{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Grep ( grepTool ) where

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lens.Micro           ( (^.), (?~), non )
import           System.Directory     ( doesDirectoryExist, doesFileExist )
import           System.FilePath.Glob ( compile, globDir1 )
import           Text.Regex.TDFA      ( (=~) )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext )

grepDescription :: Text
grepDescription = """
Fast content search tool using regular expressions.

Usage:
- Supports full regex syntax (e.g., "log.*Error", "function\\s+\\w+")
- Filter files by pattern with the include parameter (e.g., "*.hs", "*.{ts,tsx}")
- Returns file paths with line numbers, sorted by modification time
- Results limited to 100 matches

When to use:
- Searching for patterns in file contents
- Finding function/class definitions
- Locating specific strings across codebase
- Use this instead of `grep` or `rg` commands in bash
"""

grepTool :: BuiltinTool
grepTool = BuiltinTool
  { _btTool = makeTool "grep" inputSchema
      & toolDescription ?~ grepDescription
  , _btExecutor = executeGrep
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "pattern" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Regular expression pattern to search for" :: Text)
              ]
          , "path" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Directory or file to search in" :: Text)
              ]
          , "include" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Glob pattern to filter files (e.g., '*.hs')" :: Text)
              ]
          ]
      , "required" Aeson..= (["pattern"] :: [Text])
      ]

executeGrep :: ToolContext -> Aeson.Value -> IO ToolResult
executeGrep _ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (regexPattern, basePath, mInclude) -> do
      isDir <- doesDirectoryExist (toString basePath)
      isFile <- doesFileExist (toString basePath)

      if not isDir && not isFile
        then pure $ ToolResult False ("Path not found: " <> basePath)
        else do
          files <- if isFile
            then pure [toString basePath]
            else do
              let includePattern = mInclude ^. non "**/*"
              globDir1 (compile (toString includePattern)) (toString basePath)

          results <- fmap concat $ forM files $ \file -> do
            isF <- doesFileExist file
            if not isF
              then pure []
              else do
                content <- TIO.readFile file
                let lns = zip [1 :: Int ..] (T.lines content)
                    matches = filter (\(_, line) -> (toString line :: String) =~ (toString regexPattern :: String)) lns
                pure $ map (uncurry (formatMatch file)) matches

          let limited = take 100 results
          if null results
            then pure $ ToolResult True "No matches found."
            else do
              let output = T.unlines limited
                  suffix = if length results > 100
                    then "\n... (showing 100 of " <> T.pack (show (length results)) <> " matches)"
                    else ""
              pure $ ToolResult True (output <> suffix)
  where
    parseArgs = Aeson.withObject "GrepArgs" $ \o -> do
      regexPattern <- o .: "pattern"
      basePath <- (o .:? "path") <&> fromMaybe "."
      include <- o .:? "include"
      pure (regexPattern :: Text, basePath :: Text, include :: Maybe Text)

    formatMatch :: FilePath -> Int -> Text -> Text
    formatMatch file lineNum line =
      toText file <> ":" <> T.pack (show lineNum) <> ":" <> truncateLine line

    truncateLine :: Text -> Text
    truncateLine t
      | T.length t > 200 = T.take 200 t <> "..."
      | otherwise = t
