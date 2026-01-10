{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.App ( AppConfig(..), runApp, runAgentOnce, runAgentOnceStreaming ) where

import           Control.Exception            ( bracket )
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO

import           Lens.Micro                   ( (.~), (^.) )

import           Network.HTTP.Client          ( newManager )
import           Network.HTTP.Client.TLS      ( tlsManagerSettings )

import           Polysemy                     ( runM )
import           Polysemy.Error               ( runError )

import           Telos.Agent.Config           ( AgentConfig
                                              , acStreamingEnabled
                                              , acMCPServers
                                              , mscName
                                              , mscCommand
                                              , mscArgs
                                              , mscEnv
                                              )
import           Telos.Agent.Context          ( AgentContext
                                              , ctxConfig
                                              , newAgentContext
                                              , registerTools
                                              )
import           Telos.Agent.Interrupt        ( clearInterrupt, withInterruptHandler )
import           Telos.Agent.Loop             ( AgentResult(..)
                                              , runAgentLoop
                                              , runAgentLoopStreaming
                                              )
import           Telos.Core.Error             ( AppError )
import           Telos.Effect.Logger.IO       ( runLoggerIO )
import           Telos.Effect.StreamOutput.IO ( runStreamOutputIO )
import           Telos.LLM.Copilot.Auth       ( newCopilotAuth )
import           Telos.LLM.Copilot.Client     ( CopilotClient
                                              , CopilotConfig(..)
                                              , newCopilotClient
                                              )
import           Telos.LLM.Interpreter        ( runLLMWithCopilot )
import           Telos.MCP.Interpreter        ( runMCPWithManager )
import           Telos.MCP.ServerManager      ( ServerManager
                                              , twsTool
                                              , addServer
                                              , aggregateTools
                                              , newServerManager
                                              , shutdownAll
                                              )
import           Telos.MCP.Types              ( makeServerConfig
                                              , scEnv
                                              )

data AppConfig = AppConfig { appAgentConfig :: AgentConfig, appCopilotConfig :: CopilotConfig }

runApp :: AppConfig -> IO ()
runApp config = do
  bracket setup teardown $ \( ctx, client, manager ) -> do
    initializeTools ctx manager
    withInterruptHandler ctx $ repl ctx client manager
  where
    setup = do
      httpManager <- newManager tlsManagerSettings
      auth <- newCopilotAuth httpManager
      let copilotCfg = appCopilotConfig config
      let client = newCopilotClient auth httpManager copilotCfg
      serverMgr <- newServerManager
      ctx <- newAgentContext (appAgentConfig config)
      pure ( ctx, client, serverMgr )

    teardown ( _, _, manager ) = shutdownAll manager

initializeTools :: AgentContext -> ServerManager -> IO ()
initializeTools ctx manager = do
  let servers = ctx ^. ctxConfig . acMCPServers
  forM_ servers $ \serverCfg -> do
    let name      = serverCfg ^. mscName
        cmd       = serverCfg ^. mscCommand
        args      = serverCfg ^. mscArgs
        env       = serverCfg ^. mscEnv
        srvConfig
          = makeServerConfig name cmd args
          & scEnv .~ (if null env then Nothing else Just env)
    TIO.putStrLn $ "Connecting to MCP server: " <> name
    result <- addServer manager srvConfig
    case result of
      Left err -> TIO.putStrLn $ "  Failed: " <> T.pack (show err)
      Right _  -> TIO.putStrLn "  Connected"

  refreshTools ctx manager

refreshTools :: AgentContext -> ServerManager -> IO ()
refreshTools ctx manager = do
  result <- aggregateTools manager
  case result of
    Left err -> TIO.putStrLn $ "Failed to list tools: " <> T.pack (show err)
    Right toolsWithSource -> do
      let tools = map (^. twsTool) toolsWithSource
      registerTools ctx tools
      TIO.putStrLn $ "Registered " <> T.pack (show $ length tools) <> " tools"

repl :: AgentContext -> CopilotClient -> ServerManager -> IO ()
repl ctx client manager = do
  TIO.putStr "> "
  hFlush stdout
  input <- TIO.getLine
  unless (T.null input || input == "exit" || input == "quit") $ do
    clearInterrupt ctx
    -- Use streaming if enabled in config
    let streamingEnabled = ctx ^. ctxConfig . acStreamingEnabled
    result <- if streamingEnabled
      then runAgentOnceStreaming ctx client manager input
      else runAgentOnce ctx client manager input
    case result of
      AgentResponse resp ->
        -- For non-streaming, print response; for streaming, response was already printed
        unless streamingEnabled $ TIO.putStrLn resp
      AgentInterrupted partial -> TIO.putStrLn $ "\n" <> partial <> "\n[Interrupted]"
      AgentMaxIterations partial -> TIO.putStrLn $ partial <> "\n[Max iterations reached]"
      AgentError err -> TIO.putStrLn $ "[Error] " <> err
    repl ctx client manager

-- | Run agent once with non-streaming LLM calls
runAgentOnce :: AgentContext -> CopilotClient -> ServerManager -> Text -> IO AgentResult
runAgentOnce ctx client manager input = do
  result <- runM
    $ runError @AppError
    $ runLoggerIO
    $ runMCPWithManager manager
    $ runLLMWithCopilot client
    $ runAgentLoop ctx input
  case result of
    Left err -> pure $ AgentError $ T.pack (show err)
    Right r  -> pure r

-- | Run agent once with streaming LLM calls
runAgentOnceStreaming :: AgentContext -> CopilotClient -> ServerManager -> Text -> IO AgentResult
runAgentOnceStreaming ctx client manager input = do
  result <- runM
    $ runError @AppError
    $ runLoggerIO
    $ runStreamOutputIO
    $ runMCPWithManager manager
    $ runLLMWithCopilot client
    $ runAgentLoopStreaming ctx input
  case result of
    Left err -> pure $ AgentError $ T.pack (show err)
    Right r  -> pure r
