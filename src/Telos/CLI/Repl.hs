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
  , rsSessionId
  , rsAgentContext
  , rsConfig
  , rsServerManager
  , rsAuth
  ) where

import           Control.Exception           ( IOException, catch )
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Time                   ( defaultTimeLocale, formatTime, getCurrentTime )

import           Lens.Micro                  ( (.~), (^.) )

import           System.Directory            ( doesDirectoryExist, getCurrentDirectory )
import           System.IO                   ( hFlush, stdout )
import           System.Info                 ( os )

import           Telos.Agent.Config          ( acMaxIterations, acPromptConfig, makeAgentConfig )
import           Telos.Agent.Context         ( AgentContext
                                             , clearHistory
                                             , ctxConfig
                                             , getHistory
                                             , newAgentContext
                                             , registerTools
                                             , setHistory
                                             )
import           Telos.Agent.Loop            ( AgentResult(..) )
import           Telos.CLI.Config            ( CliConfig
                                             , ccMaxIterations
                                             , ccMcpServers
                                             , ccModel
                                             )
import           Telos.MCP.ServerManager     ( ServerManager
                                             , ServerStatus(..)
                                             , aggregateTools
                                             , getServerStatus
                                             , newServerManager
                                             , registerServer
                                             , shutdownAll
                                             )
import           Telos.Core.Types            ( toolDescription, toolName )
import           Telos.LLM.Copilot.Auth      ( CopilotAuth )
import           Telos.Storage.Session       ( createSession
                                             , listSessions
                                             , loadContextMessages
                                             , saveContextMessages
                                             )
import           Telos.Storage.Types         ( SessionId(..)
                                             , siId
                                             , siTitle
                                             , siUpdatedAt
                                             )
import           Telos.Tool.Registry         ( builtinToolList )
import           Telos.Prompt.Types          ( makeSystemPromptConfig )

-- | REPL state
data ReplState
  = ReplState { rsConfig        :: CliConfig
              , rsServerManager :: ServerManager
              , rsAgentContext  :: AgentContext
              , rsAuth          :: CopilotAuth
              , rsSessionId     :: Maybe SessionId
              }

-- | Create new REPL state
newReplState :: CliConfig -> CopilotAuth -> IO ReplState
newReplState config auth = do
  -- Create lazy server manager and register servers
  serverMgr <- newServerManager
  mapM_ (registerServer serverMgr) (config ^. ccMcpServers)

  -- Create agent config (prompt config will be set in ensureToolsLoaded)
  let agentConfig
        = makeAgentConfig (config ^. ccModel)
        & acMaxIterations .~ (config ^. ccMaxIterations)

  -- Create agent context (initially no tools - will be loaded lazily)
  agentCtx <- newAgentContext agentConfig

  pure
    $ ReplState
    { rsConfig        = config
    , rsServerManager = serverMgr
    , rsAgentContext  = agentCtx
    , rsAuth          = auth
    , rsSessionId     = Nothing
    }

-- | REPL commands
data ReplCommand
  = CmdQuit
  | CmdClear
  | CmdTools
  | CmdServers
  | CmdHelp
  | CmdSessions
  | CmdLoad Text
  | CmdSave (Maybe Text)
  | CmdNew
  | CmdMessage Text
  deriving stock ( Eq, Show )

-- | Parse user input into command
parseCommand :: Text -> ReplCommand
parseCommand input
  | cmd `elem` [ "/quit", "/q", "/exit" ] = CmdQuit
  | cmd == "/clear" = CmdClear
  | cmd == "/tools" = CmdTools
  | cmd == "/servers" = CmdServers
  | cmd `elem` [ "/help", "/h", "/?" ] = CmdHelp
  | cmd == "/sessions" = CmdSessions
  | "/load " `T.isPrefixOf` cmd = CmdLoad (T.strip $ T.drop 6 input)
  | "/save" `T.isPrefixOf` cmd = CmdSave (let arg = T.strip $ T.drop 5 input
                                          in if T.null arg then Nothing else Just arg)
  | cmd == "/new" = CmdNew
  | otherwise = CmdMessage input
  where
    cmd = T.toLower $ T.strip input

-- | Run the REPL loop
runRepl :: ReplState
        -> (ReplState -> Text -> IO AgentResult)
        -> IO ()
runRepl initialState runAgent = do
  printWelcome
  loop initialState
  where
    loop replState = do
      TIO.putStr "telos> "
      System.IO.hFlush System.IO.stdout
      mInput <- tryGetLine
      case mInput of
        Nothing -> do
          TIO.putStrLn ""
          TIO.putStrLn "Goodbye!"
          shutdownAll (rsServerManager replState)
        Just input -> handleInput replState input

    handleInput replState input =
      case parseCommand input of
        CmdQuit        -> do
          TIO.putStrLn "Goodbye!"
          shutdownAll (rsServerManager replState)

        CmdClear       -> do
          clearHistory (rsAgentContext replState)
          TIO.putStrLn "Conversation history cleared."
          loop replState

        CmdTools       -> do
          handleToolsCommand replState
          loop replState

        CmdServers     -> do
          handleServersCommand replState
          loop replState

        CmdHelp        -> do
          printHelp
          loop replState

        CmdSessions    -> do
          handleSessionsCommand
          loop replState

        CmdLoad prefix -> do
          replState' <- handleLoadCommand replState prefix
          loop replState'

        CmdSave mTitle -> do
          replState' <- handleSaveCommand replState mTitle
          loop replState'

        CmdNew         -> do
          replState' <- handleNewCommand replState
          loop replState'

        CmdMessage ""  -> loop replState

        CmdMessage msg -> do
          replState' <- ensureToolsLoaded replState
          result <- runAgent replState' msg
          handleAgentResult result
          replState'' <- autoSave replState'
          loop replState''

    tryGetLine :: IO (Maybe Text)
    tryGetLine = (Just <$> TIO.getLine) `catch` handleEOF

    handleEOF :: IOException -> IO (Maybe Text)
    handleEOF _ = pure Nothing

-- | Ensure tools are loaded from MCP servers and system prompt is configured
ensureToolsLoaded :: ReplState -> IO ReplState
ensureToolsLoaded replState = do
  toolsResult <- aggregateTools (rsServerManager replState)
  let mcpTools = case toolsResult of
        Left err -> do
          -- Log warning but continue with builtin tools only
          let _ = err  -- suppress unused warning
          []
        Right toolPairs -> map fst toolPairs
      allTools = builtinToolList <> mcpTools

  -- Register all tools
  registerTools (rsAgentContext replState) allTools

  -- Build SystemPromptConfig with environment info
  cwd <- getCurrentDirectory
  isGit <- doesDirectoryExist (cwd <> "/.git")
  now <- getCurrentTime
  let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" now
      platform = T.pack os
      modelId = rsConfig replState ^. ccModel
      promptConfig = makeSystemPromptConfig cwd isGit platform allTools modelId dateStr

  -- Update agent config with prompt config
  let ctx = rsAgentContext replState
  atomically $ do
    let configVar = ctx ^. ctxConfig
    modifyTVar' configVar (& acPromptConfig ?~ promptConfig)

  TIO.putStrLn $ "Loaded " <> T.pack (show (length allTools)) <> " tools."
  pure replState

-- | Handle /tools command
handleToolsCommand :: ReplState -> IO ()
handleToolsCommand replState = do
  TIO.putStrLn "Built-in tools:"
  TIO.putStrLn ""
  forM_ builtinToolList $ \tool -> do
    TIO.putStrLn $ "  " <> (tool ^. toolName) <> " [builtin]"
    case tool ^. toolDescription of
      Just desc -> TIO.putStrLn $ "    " <> T.take 80 desc
      Nothing   -> pure ()

  toolsResult <- aggregateTools (rsServerManager replState)
  case toolsResult of
    Left err        -> TIO.putStrLn $ "\nError loading MCP tools: " <> T.pack (show err)
    Right toolPairs -> do
      unless (null toolPairs) $ do
        TIO.putStrLn ""
        TIO.putStrLn "MCP tools:"
        TIO.putStrLn ""
        forM_ toolPairs $ \( tool, serverName ) -> do
          TIO.putStrLn $ "  " <> (tool ^. toolName) <> " [" <> serverName <> "]"
          case tool ^. toolDescription of
            Just desc -> TIO.putStrLn $ "    " <> T.take 80 desc
            Nothing   -> pure ()

-- | Handle /servers command
handleServersCommand :: ReplState -> IO ()
handleServersCommand replState = do
  statuses <- getServerStatus (rsServerManager replState)
  if null statuses
    then TIO.putStrLn "No MCP servers configured."
    else do
      TIO.putStrLn "MCP Servers:"
      TIO.putStrLn ""
      forM_ statuses $ \( name, serverStatus ) -> do
        let statusStr = case serverStatus of
              Registered -> "registered (not connected)"
              Connected  -> "connected"
              Failed err -> "failed: " <> err
        TIO.putStrLn $ "  " <> name <> ": " <> statusStr

handleSessionsCommand :: IO ()
handleSessionsCommand = do
  sessions <- listSessions
  if null sessions
    then TIO.putStrLn "No saved sessions."
    else do
      TIO.putStrLn "Sessions:"
      TIO.putStrLn ""
      forM_ sessions $ \s -> do
        let sid = unSessionId (s ^. siId)
            title = s ^. siTitle
            updated = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (s ^. siUpdatedAt)
        TIO.putStrLn $ "  " <> sid <> " | " <> title <> " | " <> T.pack updated

handleLoadCommand :: ReplState -> Text -> IO ReplState
handleLoadCommand replState prefix = do
  sessions <- listSessions
  let matches = filter (T.isPrefixOf prefix . unSessionId . (^. siId)) sessions
  case matches of
    [] -> do
      TIO.putStrLn $ "No session found matching: " <> prefix
      pure replState
    [s] -> do
      let sid = s ^. siId
      messages <- loadContextMessages sid
      setHistory (rsAgentContext replState) messages
      TIO.putStrLn $ "Loaded session: " <> (s ^. siTitle) <> " (" <> T.pack (show $ length messages) <> " messages)"
      pure replState { rsSessionId = Just sid }
    _ -> do
      TIO.putStrLn "Multiple sessions match. Be more specific:"
      forM_ matches $ \s ->
        TIO.putStrLn $ "  " <> unSessionId (s ^. siId)
      pure replState

handleSaveCommand :: ReplState -> Maybe Text -> IO ReplState
handleSaveCommand replState mTitle = do
  history <- getHistory (rsAgentContext replState)
  case rsSessionId replState of
    Just sid -> do
      saveContextMessages sid history
      TIO.putStrLn "Session saved."
      pure replState
    Nothing -> do
      info <- createSession mTitle
      let sid = info ^. siId
      saveContextMessages sid history
      TIO.putStrLn $ "Created and saved session: " <> unSessionId sid
      pure replState { rsSessionId = Just sid }

handleNewCommand :: ReplState -> IO ReplState
handleNewCommand replState = do
  history <- getHistory (rsAgentContext replState)
  unless (null history) $ do
    case rsSessionId replState of
      Just sid -> do
        saveContextMessages sid history
        TIO.putStrLn "Previous session saved."
      Nothing -> do
        info <- createSession Nothing
        saveContextMessages (info ^. siId) history
        TIO.putStrLn $ "Previous session saved as: " <> unSessionId (info ^. siId)

  clearHistory (rsAgentContext replState)
  TIO.putStrLn "Started new session."
  pure replState { rsSessionId = Nothing }

autoSave :: ReplState -> IO ReplState
autoSave replState = do
  case rsSessionId replState of
    Just sid -> do
      history <- getHistory (rsAgentContext replState)
      saveContextMessages sid history
      pure replState
    Nothing -> pure replState

-- | Handle agent result
handleAgentResult :: AgentResult -> IO ()
handleAgentResult = \case
  AgentResponse content -> TIO.putStrLn content  -- Print full response
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
  TIO.putStrLn "  /quit, /q      Exit Telos"
  TIO.putStrLn "  /clear         Clear conversation history"
  TIO.putStrLn "  /tools         List available tools"
  TIO.putStrLn "  /servers       Show MCP server status"
  TIO.putStrLn "  /sessions      List saved sessions"
  TIO.putStrLn "  /load <id>     Load a session (prefix match)"
  TIO.putStrLn "  /save [title]  Save current session"
  TIO.putStrLn "  /new           Start new session"
  TIO.putStrLn "  /help, /h      Show this help"
  TIO.putStrLn ""
  TIO.putStrLn "Configuration: ~/.config/telos/config.json"
  TIO.putStrLn "Sessions: ~/.local/share/telos/sessions/"

