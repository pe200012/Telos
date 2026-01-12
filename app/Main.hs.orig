{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Network.HTTP.Client      ( newManager )
import           Network.HTTP.Client.TLS  ( tlsManagerSettings )

import           Polysemy                 ( runM )
import           Polysemy.Error           ( runError )

import           Relude

import           Telos.Agent.Loop         ( AgentResult(..), runAgentLoop )
import           Telos.CLI.Config         ( configFilePath, loadConfig )
import           Telos.CLI.Repl           ( newReplState
                                          , rsAgentContext
                                          , rsAuth
                                          , rsServerManager
                                          , runRepl
                                          )
import           Telos.Core.Error         ( AppError )
import           Telos.Effect.Logger.IO   ( runLoggerIO )
import           Telos.LLM.Copilot.Auth   ( newCopilotAuth )
import           Telos.LLM.Copilot.Client ( CopilotConfig(..), newCopilotClient )
import           Telos.LLM.Interpreter    ( runLLMWithCopilot )
import           Telos.MCP.Interpreter    ( runMCPWithManager )

main :: IO ()
main = do
  -- Load configuration
  config <- loadConfig
  cfgPath <- configFilePath
  TIO.putStrLn $ "Configuration: " <> T.pack cfgPath

  -- Initialize HTTP manager and Copilot auth
  httpManager <- newManager tlsManagerSettings
  TIO.putStrLn "Authenticating with GitHub Copilot..."
  auth <- newCopilotAuth httpManager
  TIO.putStrLn "Authentication successful."

  -- Create REPL state
  replState <- newReplState config auth

  -- Run REPL with agent runner
  runRepl replState $ \replSt userInput -> do
    let copilotCfg = CopilotConfig { _ccModel = "gpt-4o", _ccMaxTokens = Just 4096 }
        client     = newCopilotClient (rsAuth replSt) httpManager copilotCfg

    result <- runM
      $ runError @AppError
      $ runLoggerIO
      $ runMCPWithManager (rsServerManager replSt)
      $ runLLMWithCopilot client
      $ runAgentLoop (rsAgentContext replSt) userInput

    case result of
      Left err -> pure $ AgentError (T.pack $ show err)
      Right r  -> pure r
