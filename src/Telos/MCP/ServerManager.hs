{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Server manager with lazy connection semantics.
-- Servers are registered but only connected on demand.
module Telos.MCP.ServerManager
  ( ServerManager
  , ServerStatus(..)
  , newServerManager
  , registerServer
  , getOrConnectServer
  , findToolServer
  , aggregateTools
  , getServerStatus
  , shutdownAll
  , getFirstConnection
  ) where

import qualified Control.Concurrent.STM as STM
import           Control.Exception      ( try )

import qualified Data.Map.Strict        as Map

import           Lens.Micro             ( (.~), (^.) )
import           Lens.Micro.TH          ( makeLenses )

import           Telos.CLI.Config       ( McpServerEntry
                                        , mseArgs
                                        , mseCommand
                                        , mseEnv
                                        , mseName
                                        , mseWorkDir
                                        )
import           Telos.Core.Error       ( MCPError(..) )
import           Telos.Core.Types       ( Tool, makeTool, toolDescription )
import           Telos.MCP.Client
import           Telos.MCP.Types

-- | Server status
data ServerStatus
  = Registered    -- ^ Config registered but not connected
  | Connected     -- ^ Active connection
  | Failed Text   -- ^ Connection failed with error
  deriving stock ( Eq, Show )

-- | Internal server state
data ServerState
  = StateRegistered McpServerEntry
  | StateConnected MCPConnection
  | StateFailed McpServerEntry Text

-- | Server manager - stores configs, connects on demand
newtype ServerManager
  = ServerManager { _smServers :: TVar (Map Text ServerState) }

makeLenses ''ServerManager

-- | Create a new server manager
newServerManager :: IO ServerManager
newServerManager = do
  serversVar <- newTVarIO Map.empty
  pure $ ServerManager { _smServers = serversVar }

-- | Register a server config without connecting
registerServer :: ServerManager -> McpServerEntry -> IO ()
registerServer mgr entry = atomically $ do
  STM.modifyTVar' (mgr ^. smServers) $ Map.insert (entry ^. mseName) (StateRegistered entry)

-- | Convert McpServerEntry to ServerConfig
toServerConfig :: McpServerEntry -> ServerConfig
toServerConfig entry
  = makeServerConfig (entry ^. mseName) (entry ^. mseCommand) (entry ^. mseArgs)
  & scWorkDir .~ (entry ^. mseWorkDir)
  & scEnv .~ (entry ^. mseEnv)

-- | Get an existing connection or connect lazily
getOrConnectServer :: ServerManager -> Text -> IO (Either MCPError MCPConnection)
getOrConnectServer mgr serverName = do
  serverState <- atomically $ do
    servers <- readTVar (mgr ^. smServers)
    pure $ Map.lookup serverName servers

  case serverState of
    Nothing -> pure $ Left $ MCPConnectionFailed $ "Server not registered: " <> serverName
    Just (StateConnected conn) -> pure $ Right conn
    Just (StateFailed _ err)
      -> pure $ Left $ MCPConnectionFailed $ "Server previously failed: " <> err
    Just (StateRegistered entry) -> do
      -- Connect now
      let config = toServerConfig entry
      result <- connectToServer config
      case result of
        Left err   -> do
          atomically
            $ STM.modifyTVar' (mgr ^. smServers)
            $ Map.insert serverName (StateFailed entry (show err))
          pure $ Left err
        Right conn -> do
          atomically
            $ STM.modifyTVar' (mgr ^. smServers)
            $ Map.insert serverName (StateConnected conn)
          pure $ Right conn

-- | Get status of all servers
getServerStatus :: ServerManager -> IO [ ( Text, ServerStatus ) ]
getServerStatus mgr = do
  servers <- readTVarIO (mgr ^. smServers)
  pure $ map toStatus $ Map.toList servers
  where
    toStatus ( name, StateRegistered _ ) = ( name, Registered )
    toStatus ( name, StateConnected _ )  = ( name, Connected )
    toStatus ( name, StateFailed _ err ) = ( name, Failed err )

-- | Find which server provides a tool, connecting lazily if needed
findToolServer :: ServerManager -> Text -> IO (Maybe ( Text, MCPConnection ))
findToolServer mgr toolName = do
  servers <- readTVarIO (mgr ^. smServers)
  findInServers $ Map.toList servers
  where
    findInServers [] = pure Nothing
    findInServers (( name, serverState ) : rest) = do
      -- Try to connect if not connected
      connResult <- case serverState of
        StateConnected conn -> pure $ Just conn
        StateRegistered _   -> do
          result <- getOrConnectServer mgr name
          pure $ either (const Nothing) Just result
        StateFailed _ _     -> pure Nothing

      case connResult of
        Nothing   -> findInServers rest
        Just conn -> do
          toolsResult <- listTools conn
          case toolsResult of
            Left _    -> findInServers rest
            Right ltr -> if any (\ti -> (ti ^. tiName) == toolName) (ltr ^. ltrTools)
              then pure $ Just ( name, conn )
              else findInServers rest

-- | Aggregate tools from all servers, connecting lazily
aggregateTools :: ServerManager -> IO (Either MCPError [ ( Tool, Text ) ])
aggregateTools mgr = do
  servers <- readTVarIO (mgr ^. smServers)
  results <- forM (Map.toList servers) $ \( name, serverState ) -> do
    connResult <- case serverState of
      StateConnected conn -> pure $ Right conn
      StateRegistered _   -> getOrConnectServer mgr name
      StateFailed _ err   -> pure $ Left $ MCPConnectionFailed err

    case connResult of
      Left err   -> pure $ Left err
      Right conn -> do
        toolsResult <- listTools conn
        case toolsResult of
          Left err  -> pure $ Left err
          Right ltr -> pure $ Right $ map (\ti -> let
                                               tool
                                                 = makeTool (ti ^. tiName) (ti ^. tiInputSchema)
                                                 & toolDescription .~ (ti ^. tiDescription)
                                             in
                                               ( tool, name )) (ltr ^. ltrTools)

  pure $ concat <$> sequence results

-- | Shutdown all connected servers
shutdownAll :: ServerManager -> IO ()
shutdownAll mgr = do
  servers <- atomically $ do
    currentServers <- readTVar (mgr ^. smServers)
    writeTVar (mgr ^. smServers) Map.empty
    pure currentServers

  forM_ (Map.elems servers) $ \case
    StateConnected conn -> safeDisconnect conn
    _ -> pass
  where
    safeDisconnect conn = do
      _ <- try @SomeException $ disconnectFromServer conn
      pass

-- | Get the first active connection (for fallback MCP operations)
getFirstConnection :: ServerManager -> IO (Maybe MCPConnection)
getFirstConnection mgr = do
  servers <- readTVarIO (mgr ^. smServers)
  let connected = [ conn | StateConnected conn <- Map.elems servers ]
  pure $ listToMaybe connected
