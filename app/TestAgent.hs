{-# LANGUAGE OverloadedStrings #-}

module TestAgent ( main ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Lens.Micro               ( (.~), (?~), (^.) )

import           Network.HTTP.Client      ( newManager )
import           Network.HTTP.Client.TLS  ( tlsManagerSettings )

import           Polysemy                 ( runM )
import           Polysemy.Error           ( runError )

import           Relude

import           Telos.Agent.Config       ( acMaxIterations
                                          , acModel
                                          , acPromptConfig
                                          , defaultAgentConfig
                                          )
import           Telos.Agent.Context      ( newAgentContext, registerTools )
import           Telos.Agent.Loop         ( AgentResult(..), runAgentLoop )
import           Telos.CLI.Config         ( makeMcpServerEntry )
import           Telos.Core.Error         ( AppError )
import           Telos.Core.Types         ( toolName )
import           Telos.Effect.Logger.IO   ( runLoggerIO )
import           Telos.LLM.Copilot.Auth   ( newCopilotAuth )
import           Telos.LLM.Copilot.Client ( CopilotConfig(..), newCopilotClient )
import           Telos.LLM.Interpreter    ( runLLMWithCopilot )
import           Telos.MCP.Interpreter    ( runMCPWithManager )
import           Telos.MCP.ServerManager  ( aggregateTools
                                          , getOrConnectServer
                                          , newServerManager
                                          , registerServer
                                          , shutdownAll
                                          )
import           Telos.Prompt.Types       ( simpleSystemPromptConfig )

main :: IO ()
main = do
  putStrLn "=== Telos Agent Test ==="

  httpManager <- newManager tlsManagerSettings
  auth <- newCopilotAuth httpManager

  let copilotCfg = CopilotConfig { _ccModel = "gpt-4o", _ccMaxTokens = Just 4096 }
      client     = newCopilotClient auth httpManager copilotCfg

  serverMgr <- newServerManager

  let fsEntry
        = makeMcpServerEntry
          "filesystem"
          "npx"
          [ "-y", "@modelcontextprotocol/server-filesystem", "/tmp" ]

  putStrLn "Registering filesystem MCP server..."
  registerServer serverMgr fsEntry

  -- Connect lazily by calling getOrConnectServer
  putStrLn "Connecting to filesystem MCP server..."
  connResult <- getOrConnectServer serverMgr "filesystem"
  case connResult of
    Left err -> putStrLn $ "Failed to connect: " ++ show err
    Right _  -> putStrLn "Connected to filesystem server"

  toolsResult <- aggregateTools serverMgr
  case toolsResult of
    Left err -> putStrLn $ "Failed to list tools: " ++ show err
    Right toolsWithSource -> do
      let tools = map fst toolsWithSource
      putStrLn $ "Available tools: " ++ show (length tools)
      mapM_ (\( tool, serverName ) -> TIO.putStrLn
             $ "  - " <> serverName <> "/" <> (tool ^. toolName)) toolsWithSource

      let agentCfg
            = defaultAgentConfig
            & acPromptConfig
            ?~ simpleSystemPromptConfig
              "You are a helpful assistant with access to filesystem tools. Be concise."
            & acModel .~ "gpt-4o"
            & acMaxIterations .~ 5

      ctx <- newAgentContext agentCfg
      registerTools ctx tools

      putStrLn "\n=== Running Agent ==="
      let testInput = "List the files in /tmp directory"
      TIO.putStrLn $ "User: " <> T.pack testInput

      result <- runM
        $ runError @AppError
        $ runLoggerIO
        $ runMCPWithManager serverMgr
        $ runLLMWithCopilot client
        $ runAgentLoop ctx (T.pack testInput)

      case result of
        Left err          -> putStrLn $ "Error: " ++ show err
        Right agentResult -> case agentResult of
          AgentResponse resp -> do
            putStrLn "\n=== Agent Response ==="
            TIO.putStrLn resp
          AgentInterrupted partial -> do
            putStrLn "\n=== Interrupted ==="
            TIO.putStrLn partial
          AgentMaxIterations partial -> do
            putStrLn "\n=== Max Iterations ==="
            TIO.putStrLn partial
          AgentError err -> putStrLn $ "Agent error: " ++ T.unpack err

  putStrLn "\nShutting down..."
  shutdownAll serverMgr
  putStrLn "Done."
