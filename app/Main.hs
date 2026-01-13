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
import           Telos.CLI.Config         ( configFilePath, loadConfig, loadTelosConfig )
import           Telos.Config.Types       ( tcModel )
import           Control.Lens             ( (^.) )
import           Telos.CLI.Repl           ( newReplState
                                           , rsAgentContext
                                           , rsProvider
                                           , rsServerManager
                                           , runRepl
                                           )
import           Telos.Core.Error         ( AppError )
import           Telos.Effect.Logger.IO   ( runLoggerIO )
import           Telos.LLM.Copilot.Auth   ( newCopilotAuth )
import           Telos.LLM.Copilot.Client ( CopilotConfig(..), newCopilotClient )
import           Telos.LLM.Interpreter    ( runLLMWithProvider )
import           Telos.LLM.Provider.Copilot ( createCopilotProvider )
import           Telos.LLM.Provider.Manager ( createProviderFromConfig )
import           Telos.MCP.Interpreter    ( runMCPWithManager )

main :: IO ()
main = do
  -- Load configuration
  config      <- loadConfig
  telosConfig <- loadTelosConfig
  cfgPath <- configFilePath
  TIO.putStrLn $ "Configuration: " <> T.pack cfgPath

  -- Initialize HTTP manager
  httpManager <- newManager tlsManagerSettings

  -- Create provider from config
  TIO.putStrLn "Creating LLM provider..."
  providerResult <- createProviderFromConfig telosConfig httpManager

  provider <- case providerResult of
    Left err -> do
      TIO.putStrLn $ "Error creating provider: " <> T.pack (show err)
      -- Fallback to Copilot for backward compatibility
      TIO.putStrLn "Falling back to GitHub Copilot..."
      auth <- newCopilotAuth httpManager
      let copilotConfig = CopilotConfig
            { _ccModel = telosConfig ^. tcModel
            , _ccMaxTokens = Just 4096
            }
          copilotClient = newCopilotClient auth httpManager copilotConfig
      createCopilotProvider copilotClient
    Right p -> do
      TIO.putStrLn "Provider created successfully."
      pure p

  -- Create REPL state
  replState <- newReplState config provider

  -- Run REPL with agent runner
  runRepl replState $ \replSt userInput -> do
    result <- runM
      $ runError @AppError
      $ runLoggerIO
      $ runMCPWithManager (rsServerManager replSt)
      $ runLLMWithProvider (rsProvider replSt)
      $ runAgentLoop (rsAgentContext replSt) userInput

    case result of
      Left err -> pure $ AgentError (T.pack $ show err)
      Right r  -> pure r
