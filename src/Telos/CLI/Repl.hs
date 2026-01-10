{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.CLI.Repl
  ( ReplState
  , newReplState
  , runRepl
  , ReplCommand(..)
  , parseCommand
  ) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Lens.Micro                ( (^.), (.~) )

import           System.IO                 ( hFlush, stdout )

import           Telos.CLI.Config          ( CliConfig, ccMcpServers, ccMaxIterations
                                           , ccModel, ccSystemPrompt )
import           Telos.CLI.LazyServerManager ( LazyServerManager, ServerStatus(..)
                                             , aggregateToolsLazy, getServerStatus
                                             , newLazyServerManager, registerServer
                                             , shutdownAllLazy )
import           Telos.Agent.Config        ( makeAgentConfig
                                           , acMaxIterations, acSystemPrompt )
import           Telos.Agent.Context       ( AgentContext, clearHistory, newAgentContext
                                           , registerTools )
import           Telos.Agent.Loop          ( AgentResult(..) )
import           Telos.Core.Types          ( toolName, toolDescription )
import           Telos.LLM.Copilot.Auth    ( CopilotAuth )

-- | REPL state
data ReplState = ReplState
  { rsConfig        :: CliConfig
  , rsServerManager :: LazyServerManager
  , rsAgentContext  :: AgentContext
  , rsAuth          :: CopilotAuth
  }

-- | Create new REPL state
newReplState :: CliConfig -> CopilotAuth -> IO ReplState
newReplState config auth = do
  -- Create lazy server manager and register servers
  serverMgr <- newLazyServerManager
  mapM_ (registerServer serverMgr) (config ^. ccMcpServers)
  
  -- Create agent config from CLI config
  let agentConfig = makeAgentConfig (config ^. ccModel)
        & acMaxIterations .~ (config ^. ccMaxIterations)
        & acSystemPrompt .~ (config ^. ccSystemPrompt)
  
  -- Create agent context (initially no tools - will be loaded lazily)
  agentCtx <- newAgentContext agentConfig
  
  pure $ ReplState
    { rsConfig = config
    , rsServerManager = serverMgr
    , rsAgentContext = agentCtx
    , rsAuth = auth
    }

-- | REPL commands
data ReplCommand
  = CmdQuit
  | CmdClear
  | CmdTools
  | CmdServers
  | CmdHelp
  | CmdMessage Text
  deriving stock (Eq, Show)

-- | Parse user input into command
parseCommand :: Text -> ReplCommand
parseCommand input
  | cmd `elem` ["/quit", "/q", "/exit"] = CmdQuit
  | cmd == "/clear" = CmdClear
  | cmd == "/tools" = CmdTools
  | cmd == "/servers" = CmdServers
  | cmd `elem` ["/help", "/h", "/?"] = CmdHelp
  | otherwise = CmdMessage input
  where
    cmd = T.toLower $ T.strip input

-- | Run the REPL loop
runRepl :: ReplState
        -> (ReplState -> Text -> IO AgentResult)  -- ^ Agent runner function
        -> IO ()
runRepl initialState runAgent = do
  printWelcome
  loop initialState
  where
    loop replState = do
      TIO.putStr "telos> "
      System.IO.hFlush System.IO.stdout
      input <- TIO.getLine
      
      case parseCommand input of
        CmdQuit -> do
          TIO.putStrLn "Goodbye!"
          shutdownAllLazy (rsServerManager replState)
        
        CmdClear -> do
          clearHistory (rsAgentContext replState)
          TIO.putStrLn "Conversation history cleared."
          loop replState
        
        CmdTools -> do
          handleToolsCommand replState
          loop replState
        
        CmdServers -> do
          handleServersCommand replState
          loop replState
        
        CmdHelp -> do
          printHelp
          loop replState
        
        CmdMessage "" -> loop replState  -- Empty input, just continue
        
        CmdMessage msg -> do
          -- Ensure tools are loaded before running agent
          replState' <- ensureToolsLoaded replState
          result <- runAgent replState' msg
          handleAgentResult result
          loop replState'

-- | Ensure tools are loaded from MCP servers
ensureToolsLoaded :: ReplState -> IO ReplState
ensureToolsLoaded replState = do
  toolsResult <- aggregateToolsLazy (rsServerManager replState)
  case toolsResult of
    Left err -> do
      TIO.putStrLn $ "Warning: Failed to load some tools: " <> T.pack (show err)
      pure replState
    Right toolPairs -> do
      let tools = map fst toolPairs
      registerTools (rsAgentContext replState) tools
      pure replState

-- | Handle /tools command
handleToolsCommand :: ReplState -> IO ()
handleToolsCommand replState = do
  toolsResult <- aggregateToolsLazy (rsServerManager replState)
  case toolsResult of
    Left err -> TIO.putStrLn $ "Error loading tools: " <> T.pack (show err)
    Right toolPairs -> do
      if null toolPairs
        then TIO.putStrLn "No tools available. Configure MCP servers in ~/.config/telos/config.json"
        else do
          TIO.putStrLn $ "Available tools (" <> T.pack (show $ length toolPairs) <> "):"
          TIO.putStrLn ""
          forM_ toolPairs $ \(tool, serverName) -> do
            TIO.putStrLn $ "  " <> (tool ^. toolName) <> " [" <> serverName <> "]"
            case tool ^. toolDescription of
              Just desc -> TIO.putStrLn $ "    " <> T.take 80 desc
              Nothing -> pure ()

-- | Handle /servers command
handleServersCommand :: ReplState -> IO ()
handleServersCommand replState = do
  statuses <- getServerStatus (rsServerManager replState)
  if null statuses
    then TIO.putStrLn "No MCP servers configured."
    else do
      TIO.putStrLn "MCP Servers:"
      TIO.putStrLn ""
      forM_ statuses $ \(name, serverStatus) -> do
        let statusStr = case serverStatus of
              Registered -> "registered (not connected)"
              Connected -> "connected"
              Failed err -> "failed: " <> err
        TIO.putStrLn $ "  " <> name <> ": " <> statusStr

-- | Handle agent result
handleAgentResult :: AgentResult -> IO ()
handleAgentResult = \case
  AgentResponse _ -> pure ()  -- Response already streamed/printed
  AgentInterrupted partial -> do
    TIO.putStrLn ""
    TIO.putStrLn $ "[Interrupted] " <> T.take 100 partial
  AgentMaxIterations partial -> do
    TIO.putStrLn ""
    TIO.putStrLn $ "[Max iterations reached] " <> T.take 100 partial
  AgentError err -> do
    TIO.putStrLn ""
    TIO.putStrLn $ "[Error] " <> err

-- | Print welcome message
printWelcome :: IO ()
printWelcome = do
  TIO.putStrLn "Telos - AI Agent with MCP Tools"
  TIO.putStrLn "Type /help for commands, or start chatting."
  TIO.putStrLn ""

-- | Print help
printHelp :: IO ()
printHelp = do
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  /quit, /q    Exit Telos"
  TIO.putStrLn "  /clear       Clear conversation history"
  TIO.putStrLn "  /tools       List available tools"
  TIO.putStrLn "  /servers     Show MCP server status"
  TIO.putStrLn "  /help, /h    Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Configuration: ~/.config/telos/config.json"

