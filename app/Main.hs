{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Lens.Micro               ( (.~), (^.) )

import           Network.HTTP.Client      ( Manager, newManager )
import           Network.HTTP.Client.TLS  ( tlsManagerSettings )

import           Polysemy                 ( runM )
import           Polysemy.Error           ( runError )

import           System.IO                ( hFlush, hIsEOF, stdin, stdout )

import           Telos.Agent.Config       ( acMaxIterations, acSystemPrompt, makeAgentConfig )
import           Telos.Agent.Context      ( AgentContext
                                          , clearHistory
                                          , newAgentContext
                                          , registerTools
                                          )
import           Telos.Agent.Loop         ( AgentResult(..), runAgentLoop )
import           Telos.CLI.Config         ( CliConfig
                                          , ccMaxIterations
                                          , ccMcpServers
                                          , ccModel
                                          , ccSystemPrompt
                                          , configFilePath
                                          , loadConfig
                                          , mseArgs
                                          , mseCommand
                                          , mseEnv
                                          , mseName
                                          , mseWorkDir
                                          )
import           Telos.Core.Error         ( AppError )
import           Telos.Core.Types         ( Tool, toolDescription, toolName )
import           Telos.Effect.Logger.IO   ( runLoggerIO )
import           Telos.LLM.Copilot.Auth   ( CopilotAuth, newCopilotAuth )
import           Telos.LLM.Copilot.Client ( CopilotConfig(..), newCopilotClient )
import           Telos.LLM.Interpreter    ( runLLMWithCopilot )
import           Telos.MCP.Interpreter    ( runMCPWithManager )
import           Telos.MCP.ServerManager  ( ServerManager
                                          , addServer
                                          , aggregateTools
                                          , newServerManager
                                          , shutdownAll
                                          , twsTool
                                          )
import           Telos.MCP.Types          ( makeServerConfig, scEnv, scWorkDir )

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

  -- Create server manager and connect configured servers
  serverMgr <- newServerManager
  forM_ (config ^. ccMcpServers) $ \entry -> do
    let serverCfg
          = makeServerConfig (entry ^. mseName) (entry ^. mseCommand) (entry ^. mseArgs)
          & scWorkDir .~ (entry ^. mseWorkDir)
          & scEnv .~ (entry ^. mseEnv)
    TIO.putStrLn $ "Connecting to MCP server: " <> (entry ^. mseName)
    result <- addServer serverMgr serverCfg
    case result of
      Left err -> TIO.putStrLn $ "  Failed: " <> T.pack (show err)
      Right _  -> TIO.putStrLn "  Connected."

  -- Get tools from all servers
  toolsResult <- aggregateTools serverMgr
  let tools = map (^. twsTool) $ fromRight [] toolsResult
  TIO.putStrLn $ "Loaded " <> T.pack (show $ length tools) <> " tools."

  -- Create agent context
  let agentConfig
        = makeAgentConfig (config ^. ccModel)
        & acMaxIterations .~ (config ^. ccMaxIterations)
        & acSystemPrompt .~ (config ^. ccSystemPrompt)
  agentCtx <- newAgentContext agentConfig
  registerTools agentCtx tools

  -- Run REPL
  TIO.putStrLn ""
  TIO.putStrLn "Telos - AI Agent with MCP Tools"
  TIO.putStrLn "Type /help for commands, or start chatting."
  TIO.putStrLn ""

  repl httpManager serverMgr config auth agentCtx

  -- Cleanup
  shutdownAll serverMgr

-- | REPL loop
repl :: Manager -> ServerManager -> CliConfig -> CopilotAuth -> AgentContext -> IO ()
repl httpManager serverMgr config auth ctx = loop
  where
    loop = do
      TIO.putStr "telos> "
      System.IO.hFlush System.IO.stdout
      eof <- System.IO.hIsEOF System.IO.stdin
      if eof
        then TIO.putStrLn "\nGoodbye!"
        else do
          input <- TIO.getLine
          
          let cmd = T.toLower $ T.strip input
          
          if
            | cmd `elem` [ "/quit", "/q", "/exit" ] -> do
              TIO.putStrLn "Goodbye!"
            
            | cmd == "/clear" -> do
              clearHistory ctx
              TIO.putStrLn "Conversation history cleared."
              loop
            
            | cmd == "/tools" -> do
              toolsResult <- aggregateTools serverMgr
              handleToolsCommand $ map (^. twsTool) $ fromRight [] toolsResult
              loop
            
            | cmd `elem` [ "/help", "/h", "/?" ] -> do
              printHelp
              loop
            
            | T.strip input == "" -> loop
            
            | otherwise -> do
              runAgent httpManager serverMgr config auth ctx input
              loop

-- | Run agent on user input
runAgent :: Manager -> ServerManager -> CliConfig -> CopilotAuth -> AgentContext -> Text -> IO ()
runAgent httpManager serverMgr config auth ctx userInput = do
  let copilotCfg = CopilotConfig { _ccModel = config ^. ccModel, _ccMaxTokens = Just 4096 }
      client     = newCopilotClient auth httpManager copilotCfg

  result <- runM
    $ runError @AppError
    $ runLoggerIO
    $ runMCPWithManager serverMgr
    $ runLLMWithCopilot client
    $ runAgentLoop ctx userInput

  case result of
    Left err          -> TIO.putStrLn $ "[Error] " <> T.pack (show err)
    Right agentResult -> case agentResult of
      AgentResponse resp -> TIO.putStrLn resp
      AgentInterrupted partial -> TIO.putStrLn $ "[Interrupted] " <> partial
      AgentMaxIterations partial -> TIO.putStrLn $ "[Max iterations] " <> partial
      AgentError err -> TIO.putStrLn $ "[Agent Error] " <> err

-- | Handle /tools command
handleToolsCommand :: [ Tool ] -> IO ()
handleToolsCommand tools = do
  if null tools
    then TIO.putStrLn "No tools available. Configure MCP servers in ~/.config/telos/config.json"
    else do
      TIO.putStrLn $ "Available tools (" <> T.pack (show $ length tools) <> "):"
      TIO.putStrLn ""
      forM_ tools $ \tool -> do
        TIO.putStrLn $ "  " <> (tool ^. toolName)
        case tool ^. toolDescription of
          Just desc -> TIO.putStrLn $ "    " <> T.take 80 desc
          Nothing   -> pass

-- | Print help
printHelp :: IO ()
printHelp = do
  TIO.putStrLn "Commands:"
  TIO.putStrLn "  /quit, /q    Exit Telos"
  TIO.putStrLn "  /clear       Clear conversation history"
  TIO.putStrLn "  /tools       List available tools"
  TIO.putStrLn "  /help, /h    Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Configuration: ~/.config/telos/config.json"
