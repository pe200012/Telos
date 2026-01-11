{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Read ( readTool ) where

import           Relude


import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Lens.Micro           ( (?~) )
import           System.Directory     ( doesFileExist, getModificationTime )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext, ToolExecutorType(..), markFileRead )

readDescription :: Text
readDescription = """
Reads a file from the local filesystem.

Usage:
- The path parameter must be an absolute path
- By default, reads up to 2000 lines from the beginning
- Use offset and limit for long files
- Lines longer than 2000 characters will be truncated
- Results returned with line numbers (1-based)

Tips:
- Speculatively read multiple related files in batch
- Can read any file type (text, code, config)
- Binary files will be rejected
"""

readTool :: BuiltinTool
readTool = BuiltinTool
  { _btTool = makeTool "read" inputSchema
      & toolDescription ?~ readDescription
  , _btExecutor = SimpleExecutor executeRead
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "path" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Absolute path to the file to read" :: Text)
              ]
          , "offset" Aeson..= Aeson.object
              [ "type" Aeson..= ("integer" :: Text)
              , "description" Aeson..= ("Line number to start reading from (0-based)" :: Text)
              ]
          , "limit" Aeson..= Aeson.object
              [ "type" Aeson..= ("integer" :: Text)
              , "description" Aeson..= ("Maximum number of lines to read (default: 2000)" :: Text)
              ]
          ]
      , "required" Aeson..= (["path"] :: [Text])
      ]

executeRead :: ToolContext -> Aeson.Value -> IO ToolResult
executeRead ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (path, offset, limit) -> do
      let filePath = toString path
      exists <- doesFileExist filePath
      if not exists
        then pure $ ToolResult False ("File not found: " <> path)
        else do
          bytes <- BS.readFile filePath
          if isBinary bytes
            then pure $ ToolResult False ("Cannot read binary file: " <> path)
            else do
              modTime <- getModificationTime filePath
              markFileRead ctx filePath (Just modTime)
              let content = TE.decodeUtf8Lenient bytes
                  lns = T.lines content
                  selectedLines = take limit $ drop offset lns
                  numbered = zipWith formatLine [offset + 1 ..] selectedLines
                  output = T.unlines numbered
              pure $ ToolResult True output
  where
    parseArgs = Aeson.withObject "ReadArgs" $ \o -> do
      path <- o .: "path"
      offset <- (o .:? "offset") <&> fromMaybe 0
      limit <- (o .:? "limit") <&> fromMaybe 2000
      pure (path :: Text, offset :: Int, limit :: Int)

    formatLine :: Int -> Text -> Text
    formatLine n line = T.pack (show n) <> "\t" <> truncateLine line

    truncateLine :: Text -> Text
    truncateLine t
      | T.length t > 2000 = T.take 2000 t <> "..."
      | otherwise = t

    isBinary :: BS.ByteString -> Bool
    isBinary bs = BS.count 0 (BS.take 8192 bs) > 0
