{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Edit ( editTool ) where

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Algorithm.Diff  as Diff
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lens.Micro           ( (?~) )
import           System.Directory     ( doesFileExist )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext, wasFileRead )

editDescription :: Text
editDescription = """
Performs exact string replacements in files.

Usage:
- You must use Read tool first before editing
- Preserve exact indentation from the file
- The edit will FAIL if oldString is not found
- The edit will FAIL if oldString matches multiple times (use replaceAll or add more context)

Parameters:
- oldString: The exact text to find and replace
- newString: The replacement text (must differ from oldString)
- replaceAll: Set true to replace all occurrences (useful for renaming)

Tips:
- Always prefer editing existing files over writing new ones
- Use replaceAll for variable/function renaming across a file
"""

editTool :: BuiltinTool
editTool = BuiltinTool
  { _btTool = makeTool "edit" inputSchema
      & toolDescription ?~ editDescription
  , _btExecutor = executeEdit
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "path" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Absolute path to the file to edit" :: Text)
              ]
          , "oldString" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("The exact string to find and replace" :: Text)
              ]
          , "newString" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("The string to replace oldString with" :: Text)
              ]
          , "replaceAll" Aeson..= Aeson.object
              [ "type" Aeson..= ("boolean" :: Text)
              , "description" Aeson..= ("Replace all occurrences (default: false)" :: Text)
              ]
          ]
      , "required" Aeson..= (["path", "oldString", "newString"] :: [Text])
      ]

executeEdit :: ToolContext -> Aeson.Value -> IO ToolResult
executeEdit ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (path, oldStr, newStr, replaceAll) -> do
      let filePath = toString path
      exists <- doesFileExist filePath
      if not exists
        then pure $ ToolResult False ("File not found: " <> path)
        else do
          wasRead <- wasFileRead ctx filePath
          if not wasRead
            then pure $ ToolResult False ("You must Read the file before editing it: " <> path)
            else if oldStr == newStr
              then pure $ ToolResult False "oldString and newString must be different"
              else do
                content <- TIO.readFile filePath
                let occurrences = countOccurrences oldStr content
                if occurrences == 0
                  then pure $ ToolResult False "oldString not found in file"
                  else if occurrences > 1 && not replaceAll
                    then pure $ ToolResult False
                      ("oldString found " <> T.pack (show occurrences) <> " times. Use replaceAll=true or provide more context.")
                    else do
                      let newContent = if replaceAll
                            then T.replace oldStr newStr content
                            else replaceFirst oldStr newStr content
                          diff = generateDiff path content newContent
                      TIO.writeFile filePath newContent
                      let header = if replaceAll
                            then "Replaced " <> T.pack (show occurrences) <> " occurrences\n\n"
                            else "Replaced 1 occurrence\n\n"
                      pure $ ToolResult True (header <> diff)
  where
    parseArgs = Aeson.withObject "EditArgs" $ \o -> do
      path <- o .: "path"
      oldStr <- o .: "oldString"
      newStr <- o .: "newString"
      replaceAll <- (o .:? "replaceAll") <&> fromMaybe False
      pure (path :: Text, oldStr :: Text, newStr :: Text, replaceAll :: Bool)

    countOccurrences :: Text -> Text -> Int
    countOccurrences needle haystack
      | T.null needle = 0
      | otherwise = length $ T.breakOnAll needle haystack

    replaceFirst :: Text -> Text -> Text -> Text
    replaceFirst needle replacement haystack =
      case T.breakOn needle haystack of
        (before, match)
          | T.null match -> haystack
          | otherwise -> before <> replacement <> T.drop (T.length needle) match

    generateDiff :: Text -> Text -> Text -> Text
    generateDiff path oldContent newContent =
      let oldLines = T.lines oldContent
          newLines = T.lines newContent
          diffs = Diff.getGroupedDiff (map toString oldLines) (map toString newLines)
          header = "--- " <> path <> "\n+++ " <> path <> "\n"
      in header <> formatDiffs diffs

    formatDiffs :: [Diff.Diff [String]] -> Text
    formatDiffs = T.concat . concatMap formatDiff
      where
        formatDiff (Diff.First old) = map (\l -> "-" <> toText l <> "\n") old
        formatDiff (Diff.Second new) = map (\l -> "+" <> toText l <> "\n") new
        formatDiff (Diff.Both ctxLines _ctxLines2) =
          let context = take 3 ctxLines
          in map (\l -> " " <> toText l <> "\n") context
