{-# LANGUAGE MultilineStrings #-}

module Telos.Tool.Bash ( bashTool ) where

import           Relude

import           Control.Concurrent.Async ( async, wait )
import           Control.Exception        ( try )

import qualified Data.Aeson           as Aeson
import           Data.Aeson           ( (.:), (.:?) )
import           Data.Aeson.Types     ( parseEither )
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Control.Lens           ( (?~), (^.), non )

import           System.Directory     ( doesDirectoryExist )
import           System.Exit          ( ExitCode(..) )
import           System.Process       ( CreateProcess(..)
                                      , StdStream(..)
                                      , createProcess
                                      , proc
                                      , waitForProcess
                                      )
import           System.Timeout       ( timeout )

import           Telos.Core.Types     ( makeTool, toolDescription )
import           Telos.Tool.Types     ( BuiltinTool(..)
                                      , StreamCallback
                                      , ToolContext
                                      , ToolExecutorType(..)
                                      , ToolResult(..)
                                      )

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
  , _btExecutor = StreamingExecutor executeBashStreaming
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

-- | Execute bash command with streaming output
executeBashStreaming :: StreamCallback -> ToolContext -> Aeson.Value -> IO ToolResult
executeBashStreaming onChunk _ctx args = do
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
              baseProc = (proc "bash" ["-c", toString cmd])
                { std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = toString <$> mWorkdir
                }

          mResult <- timeout timeoutUs $ runStreamingProcess baseProc onChunk

          case mResult of
            Nothing -> pure $ ToolResult False "Command timed out"
            Just (exitCode, output) -> do
              let success = exitCode == ExitSuccess
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

-- | Run a process and stream its output through the callback
runStreamingProcess :: CreateProcess -> StreamCallback -> IO (ExitCode, Text)
runStreamingProcess cp onChunk = do
  result <- try @SomeException $ createProcess cp
  case result of
    Left err -> pure (ExitFailure 1, "Failed to start process: " <> T.pack (show err))
    Right (_, mStdout, mStderr, ph) -> do
      -- Accumulator for full output
      outputVar <- newTVarIO ""

      -- Reader for a handle - reads lines and streams them
      let readHandle mh = case mh of
            Nothing -> pure ()
            Just h -> do
              hSetBuffering h LineBuffering
              let loop = do
                    eof <- hIsEOF h
                    unless eof $ do
                      line <- TIO.hGetLine h
                      let chunk = line <> "\n"
                      onChunk chunk  -- Stream to user
                      atomically $ modifyTVar' outputVar (<> chunk)
                      loop
              loop

      -- Read stdout and stderr concurrently
      stdoutThread <- async $ readHandle mStdout
      stderrThread <- async $ readHandle mStderr

      -- Wait for both readers to complete
      wait stdoutThread
      wait stderrThread

      -- Wait for process to exit
      exitCode <- waitForProcess ph
      output <- readTVarIO outputVar

      pure (exitCode, output)
