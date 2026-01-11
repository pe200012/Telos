{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Write ( writeTool ) where

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lens.Micro           ( (?~) )
import           System.Directory     ( createDirectoryIfMissing, doesFileExist )
import           System.FilePath      ( takeDirectory )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext, wasFileRead )

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
  , _btExecutor = executeWrite
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
          wasRead <- wasFileRead ctx filePath
          if not wasRead
            then pure $ ToolResult False ("You must Read the file before overwriting it: " <> path)
            else doWrite filePath path content
        else doWrite filePath path content
  where
    doWrite filePath path content = do
      let dir = takeDirectory filePath
      createDirectoryIfMissing True dir
      TIO.writeFile filePath content
      pure $ ToolResult True ("Wrote " <> T.pack (show (T.length content)) <> " chars to " <> path)

    parseArgs = Aeson.withObject "WriteArgs" $ \o -> do
      path <- o .: "path"
      content <- o .: "content"
      pure (path :: Text, content :: Text)
