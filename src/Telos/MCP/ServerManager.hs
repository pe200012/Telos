module Telos.MCP.ServerManager
  ( ServerManager
  , ToolWithSource(..)
  , newServerManager
  , addServer
  , removeServer
  , getConnection
  , getAllConnections
  , shutdownAll
  , aggregateTools
  , findToolServer
  ) where

import qualified Control.Concurrent.STM as STM
import           Control.Exception      ( try )

import qualified Data.Map.Strict        as Map

import           Telos.Core.Error       ( MCPError(..) )
import           Telos.Core.Types       ( Tool(..) )
import           Telos.MCP.Client
import           Telos.MCP.Types

newtype ServerManager = ServerManager { smConnections :: TVar (Map Text MCPConnection) }

newServerManager :: IO ServerManager
newServerManager = do
  connVar <- newTVarIO Map.empty
  pure $ ServerManager { smConnections = connVar }

addServer :: ServerManager -> ServerConfig -> IO (Either MCPError MCPConnection)
addServer mgr config = do
  existing <- atomically $ do
    conns <- readTVar (smConnections mgr)
    pure $ Map.lookup (scName config) conns

  case existing of
    Just conn -> pure $ Right conn
    Nothing   -> do
      result <- connectToServer config
      case result of
        Left err   -> pure $ Left err
        Right conn -> do
          atomically $ STM.modifyTVar' (smConnections mgr) (Map.insert (scName config) conn)
          pure $ Right conn

removeServer :: ServerManager -> Text -> IO ()
removeServer mgr srvName = do
  mConn <- atomically $ do
    conns <- readTVar (smConnections mgr)
    let foundConn = Map.lookup srvName conns
    writeTVar (smConnections mgr) (Map.delete srvName conns)
    pure foundConn

  forM_ mConn disconnectFromServer

getConnection :: ServerManager -> Text -> IO (Maybe MCPConnection)
getConnection mgr srvName = atomically $ do
  conns <- readTVar (smConnections mgr)
  pure $ Map.lookup srvName conns

getAllConnections :: ServerManager -> IO [ MCPConnection ]
getAllConnections mgr = atomically $ do
  conns <- readTVar (smConnections mgr)
  pure $ Map.elems conns

shutdownAll :: ServerManager -> IO ()
shutdownAll mgr = do
  conns <- atomically $ do
    currentConns <- readTVar (smConnections mgr)
    writeTVar (smConnections mgr) Map.empty
    pure $ Map.elems currentConns

  mapM_ safeDisconnect conns
  where
    safeDisconnect conn = do
      _ <- try @SomeException $ disconnectFromServer conn
      pass

data ToolWithSource = ToolWithSource { twsTool :: Tool, twsServerName :: Text }

aggregateTools :: ServerManager -> IO (Either MCPError [ ToolWithSource ])
aggregateTools mgr = do
  conns <- getAllConnections mgr
  results <- mapM fetchToolsFromServer conns
  pure $ combineResults results
  where
    fetchToolsFromServer :: MCPConnection -> IO (Either MCPError [ ToolWithSource ])
    fetchToolsFromServer conn = do
      result <- listTools conn
      case result of
        Left err  -> pure $ Left err
        Right ltr -> pure
          $ Right
          $ map (\ti -> ToolWithSource
                 { twsTool       = Tool { toolName        = tiName ti
                                        , toolDescription = tiDescription ti
                                        , toolInputSchema = tiInputSchema ti
                                        }
                 , twsServerName = mcName conn
                 }) (ltrTools ltr)

    combineResults :: [ Either MCPError [ ToolWithSource ] ] -> Either MCPError [ ToolWithSource ]
    combineResults = fmap concat . sequence

findToolServer :: ServerManager -> Text -> IO (Maybe MCPConnection)
findToolServer mgr tName = do
  conns <- getAllConnections mgr
  findInConnections conns
  where
    findInConnections [] = pure Nothing
    findInConnections (conn : rest) = do
      result <- listTools conn
      case result of
        Left _    -> findInConnections rest
        Right ltr -> if any (\ti -> tiName ti == tName) (ltrTools ltr)
          then pure $ Just conn
          else findInConnections rest
