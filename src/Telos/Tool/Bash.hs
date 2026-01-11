{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Bash ( bashTool ) where

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import           Lens.Micro           ( (?~), (^.), non )
import           Control.Exception    ( try )
import           System.Directory     ( doesDirectoryExist )
import           System.Exit          ( ExitCode(..) )
import           System.Process.Typed ( proc
                                      , readProcess
                                      , setWorkingDir
                                      )
import           System.Timeout       ( timeout )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..), ToolResult(..), ToolContext )

bashDescription :: Text
bashDescription = """
Executes a bash command with optional timeout and working directory.

Usage:
- Use `workdir` parameter instead of `cd <dir> && <cmd>`
- Default timeout: 120000ms (2 minutes)
- Output truncated at 50000 characters

Prefer specialized tools over bash:
- File search: Use Glob (not find/ls)
- Content search: Use Grep (not grep/rg)
- Read files: Use Read (not cat/head/tail)
- Edit files: Use Edit (not sed/awk)
- Write files: Use Write (not echo >)

For multiple commands:
- Independent commands: make parallel tool calls
- Dependent commands: chain with && in single call
"""

bashTool :: BuiltinTool
bashTool = BuiltinTool
  { _btTool = makeTool "bash" inputSchema
      & toolDescription ?~ bashDescription
  , _btExecutor = executeBash
  }
  where
    inputSchema = Aeson.object
      [ "type" Aeson..= ("object" :: Text)
      , "properties" Aeson..= Aeson.object
          [ "command" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("The bash command to execute" :: Text)
              ]
          , "workdir" Aeson..= Aeson.object
              [ "type" Aeson..= ("string" :: Text)
              , "description" Aeson..= ("Working directory for the command" :: Text)
              ]
          , "timeout" Aeson..= Aeson.object
              [ "type" Aeson..= ("integer" :: Text)
              , "description" Aeson..= ("Timeout in milliseconds (default: 120000)" :: Text)
              ]
          ]
      , "required" Aeson..= (["command"] :: [Text])
      ]

executeBash :: ToolContext -> Aeson.Value -> IO ToolResult
executeBash _ctx args = do
  case parseEither parseArgs args of
    Left err -> pure $ ToolResult False ("Invalid arguments: " <> T.pack err)
    Right (cmd, mWorkdir, mTimeout) -> do
      -- Validate workdir exists if specified
      wdValid <- case mWorkdir of
        Nothing -> pure (Right ())
        Just wd -> do
          exists <- doesDirectoryExist (toString wd)
          pure $ if exists then Right () else Left wd

      case wdValid of
        Left wd -> pure $ ToolResult False ("Working directory not found: " <> wd)
        Right () -> do
          let timeoutMs = mTimeout ^. non 120000
              timeoutUs = timeoutMs * 1000
              baseProc = proc "bash" ["-c", toString cmd]
              procConfig = case mWorkdir of
                Nothing -> baseProc
                Just wd -> setWorkingDir (toString wd) baseProc

          mResult <- timeout timeoutUs $ try @SomeException $ readProcess procConfig

          case mResult of
            Nothing -> pure $ ToolResult False "Command timed out"
            Just (Left _) -> pure $ ToolResult False "Command timed out"
            Just (Right (exitCode, stdoutBs, stderrBs)) -> do
              let output = decodeUtf8 stdoutBs <> decodeUtf8 stderrBs
                  success = exitCode == ExitSuccess
              pure $ ToolResult success (truncateOutput output)
  where
    parseArgs = Aeson.withObject "BashArgs" $ \o -> do
      cmd <- o .: "command"
      workdir <- o .:? "workdir"
      timeoutVal <- o .:? "timeout"
      pure (cmd :: Text, workdir :: Maybe Text, timeoutVal :: Maybe Int)

    truncateOutput :: Text -> Text
    truncateOutput t
      | T.length t > 50000 = T.take 50000 t <> "\n... (output truncated)"
      | otherwise = t
