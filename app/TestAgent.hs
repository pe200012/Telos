{-# LANGUAGE OverloadedStrings #-}

module TestAgent (main) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Network.HTTP.Client       ( newManager )
import           Network.HTTP.Client.TLS   ( tlsManagerSettings )

import           Polysemy                  ( runM )
import           Polysemy.Error            ( runError )

import           Telos.Agent.Config        ( AgentConfig(..), defaultAgentConfig )
import           Telos.Agent.Context       ( newAgentContext, registerTools )
import           Telos.Agent.Loop          ( AgentResult(..), runAgentLoop )
import           Telos.Core.Error          ( AppError )
import           Telos.Core.Types          ( Tool(..) )
import           Telos.Effect.Logger.IO    ( runLoggerIO )
import           Telos.LLM.Copilot.Auth    ( newCopilotAuth )
import           Telos.LLM.Copilot.Client  ( CopilotConfig(..), newCopilotClient )
import           Telos.LLM.Interpreter     ( runLLMWithCopilot )
import           Telos.MCP.Interpreter     ( runMCPWithManager )
import           Telos.MCP.ServerManager   ( ToolWithSource(..), addServer, aggregateTools, newServerManager, shutdownAll )
import           Telos.MCP.Types           ( ServerConfig(..) )

main :: IO ()
main = do
  putStrLn "=== Telos Agent Test ==="
  
  httpManager <- newManager tlsManagerSettings
  auth <- newCopilotAuth httpManager
  
  let copilotCfg = CopilotConfig
        { ccModel = "gpt-4o"
        , ccMaxTokens = Just 4096
        }
      client = newCopilotClient auth httpManager copilotCfg
  
  serverMgr <- newServerManager
  
  let fsConfig = ServerConfig
        { scName = "filesystem"
        , scCommand = "npx"
        , scArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
        , scEnv = Nothing
        , scWorkDir = Nothing
        }
  
  putStrLn "Connecting to filesystem MCP server..."
  fsResult <- addServer serverMgr fsConfig
  case fsResult of
    Left err -> putStrLn $ "Failed to connect: " ++ show err
    Right _  -> putStrLn "Connected to filesystem server"
  
  toolsResult <- aggregateTools serverMgr
  case toolsResult of
    Left err -> putStrLn $ "Failed to list tools: " ++ show err
    Right toolsWithSource -> do
      let tools = map twsTool toolsWithSource
      putStrLn $ "Available tools: " ++ show (length tools)
      mapM_ (\tws -> TIO.putStrLn $ "  - " <> twsServerName tws <> "/" <> toolName (twsTool tws)) toolsWithSource
      
      let agentCfg = defaultAgentConfig
            { acSystemPrompt = Just "You are a helpful assistant with access to filesystem tools. Be concise."
            , acModel = "gpt-4o"
            , acMaxIterations = 5
            }
      
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
        Left err -> putStrLn $ "Error: " ++ show err
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
