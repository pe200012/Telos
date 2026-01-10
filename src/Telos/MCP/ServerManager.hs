{-# LANGUAGE TemplateHaskell #-}

module Telos.MCP.ServerManager
  ( ServerManager
  , smConnections
  , ToolWithSource
  , twsTool
  , twsServerName
  , makeToolWithSource
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

import           Lens.Micro             ( (.~), (^.) )
import           Lens.Micro.TH          ( makeLenses )

import           Telos.Core.Error       ( MCPError(..) )
import           Telos.Core.Types       ( Tool, makeTool, toolDescription )
import           Telos.MCP.Client
import           Telos.MCP.Types

newtype ServerManager = ServerManager { _smConnections :: TVar (Map Text MCPConnection) }

makeLenses ''ServerManager

data ToolWithSource = ToolWithSource
  { _twsTool       :: Tool
  , _twsServerName :: Text
  }

makeLenses ''ToolWithSource

makeToolWithSource :: Tool -> Text -> ToolWithSource
makeToolWithSource tool srvName = ToolWithSource
  { _twsTool = tool
  , _twsServerName = srvName
  }

newServerManager :: IO ServerManager
newServerManager = do
  connVar <- newTVarIO Map.empty
  pure $ ServerManager { _smConnections = connVar }

addServer :: ServerManager -> ServerConfig -> IO (Either MCPError MCPConnection)
addServer mgr config = do
  existing <- atomically $ do
    conns <- readTVar (mgr ^. smConnections)
    pure $ Map.lookup (config ^. scName) conns

  case existing of
    Just conn -> pure $ Right conn
    Nothing   -> do
      result <- connectToServer config
      case result of
        Left err   -> pure $ Left err
        Right conn -> do
          atomically $ STM.modifyTVar' (mgr ^. smConnections) (Map.insert (config ^. scName) conn)
          pure $ Right conn

removeServer :: ServerManager -> Text -> IO ()
removeServer mgr srvName = do
  mConn <- atomically $ do
    conns <- readTVar (mgr ^. smConnections)
    let foundConn = Map.lookup srvName conns
    writeTVar (mgr ^. smConnections) (Map.delete srvName conns)
    pure foundConn

  forM_ mConn disconnectFromServer

getConnection :: ServerManager -> Text -> IO (Maybe MCPConnection)
getConnection mgr srvName = atomically $ do
  conns <- readTVar (mgr ^. smConnections)
  pure $ Map.lookup srvName conns

getAllConnections :: ServerManager -> IO [ MCPConnection ]
getAllConnections mgr = atomically $ do
  conns <- readTVar (mgr ^. smConnections)
  pure $ Map.elems conns

shutdownAll :: ServerManager -> IO ()
shutdownAll mgr = do
  conns <- atomically $ do
    currentConns <- readTVar (mgr ^. smConnections)
    writeTVar (mgr ^. smConnections) Map.empty
    pure $ Map.elems currentConns

  mapM_ safeDisconnect conns
  where
    safeDisconnect conn = do
      _ <- try @SomeException $ disconnectFromServer conn
      pass

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
          $ map (\ti -> let
              tool = makeTool (ti ^. tiName) (ti ^. tiInputSchema)
                       & toolDescription .~ (ti ^. tiDescription)
            in makeToolWithSource tool (conn ^. mcName)
          ) (ltr ^. ltrTools)

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
        Right ltr -> if any (\ti -> (ti ^. tiName) == tName) (ltr ^. ltrTools)
          then pure $ Just conn
          else findInConnections rest
