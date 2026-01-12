{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Write ( writeTool ) where

import           Relude


import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lens.Micro           ( (?~) )
import           System.Directory     ( createDirectoryIfMissing, doesFileExist, getModificationTime )
import           System.FilePath      ( takeDirectory )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..)
                                      , FileAssertError(..)
                                      , ToolContext
                                      , ToolExecutorType(..)
                                      , ToolResult(..)
                                      , assertFileRead
                                      , markFileRead
                                      )

writeDescription :: Text
writeDescription = """
Writes content to a file on the local filesystem.

Usage:
- This tool will overwrite existing files
- For existing files, prefer using Edit tool instead
- Parent directories are created automatically if needed
- Path must be an absolute path

Guidelines:
- NEVER proactively create documentation files (*.md, README)
- Only create new files when explicitly required
- Use Read tool first if you need to check existing content
"""

writeTool :: BuiltinTool
writeTool = BuiltinTool
  { _btTool = makeTool "write" inputSchema
      & toolDescription ?~ writeDescription
  , _btExecutor = SimpleExecutor executeWrite
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "path" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Absolute path to the file to write" :: Text)
              ]
          , "content" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Content to write to the file" :: Text)
              ]
          ]
      , "required" Aeson..= (["path", "content"] :: [Text])
      ]

executeWrite :: ToolContext -> Aeson.Value -> IO ToolResult
executeWrite ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (path, content) -> do
      let filePath = toString path
      exists <- doesFileExist filePath
      if exists
        then do
          -- Assert file was read and not modified externally
          assertResult <- assertFileRead ctx filePath
          case assertResult of
            Left (FileNotRead _) -> 
              pure $ ToolResult False ("You must Read the file before overwriting it. File: " <> path)
            Left (FileModifiedSinceRead _ mtime rtime) ->
              pure $ ToolResult False $
                "File " <> path <> " has been modified since it was last read.\n" <>
                "Last modification: " <> show mtime <> "\n" <>
                "Last read: " <> show rtime <> "\n\n" <>
                "Please read the file again before modifying it."
            Right () -> doWrite filePath path content
        else doWrite filePath path content
  where
    doWrite filePath path content = do
      let dir = takeDirectory filePath
      createDirectoryIfMissing True dir
      TIO.writeFile filePath content
      -- Update read time after successful write
      markFileRead ctx filePath . Just =<< getModificationTime filePath
      pure $ ToolResult True ("Wrote " <> T.pack (show (T.length content)) <> " chars to " <> path)

    parseArgs = Aeson.withObject "WriteArgs" $ \o -> do
      path <- o .: "path"
      content <- o .: "content"
      pure (path :: Text, content :: Text)
