{-# LANGUAGE TemplateHaskell #-}

module Telos.MCP.Client
  ( MCPConnection(..)
  , mcName
  , mcHandle
  , mcNextId
  , mcCapabilities
  , mcServerInfo
  , mcPendingReqs
  , mcDispatcherThread
  , connectToServer
  , disconnectFromServer
  , sendRequest
  , sendNotification
  , callTool
  , listTools
  , listResources
  , readResource
  ) where

import           Control.Concurrent        ( ThreadId, forkIO, killThread )
import qualified Control.Concurrent.STM    as STM
import           Control.Exception         ( try )
import           Control.Lens              ( (.~), (?~), (^.), makeLenses )

import           Data.Aeson                ( FromJSON
                                           , Result(..)
                                           , ToJSON
                                           , Value
                                           , fromJSON
                                           , toJSON
                                           )
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T

import           Relude

import           Telos.Core.Error          ( MCPError(..) )
import           Telos.MCP.JsonRpc
import           Telos.MCP.Transport.StdIO
import           Telos.MCP.Types

data MCPConnection
  = MCPConnection
  { _mcName :: Text
  , _mcHandle :: StdIOHandle
  , _mcNextId :: TVar Int
  , _mcCapabilities :: ServerCapabilities
  , _mcServerInfo :: Maybe ServerInfo
  , _mcPendingReqs :: TVar (Map Int (TMVar (Either MCPError JsonRpcResponse)))
  , _mcDispatcherThread :: ThreadId
  }

makeLenses ''MCPConnection

defaultClientCapabilities :: ClientCapabilities
defaultClientCapabilities
  = makeClientCapabilities & ccRoots ?~ (makeRootsCapability & rcListChanged ?~ True)

defaultClientInfo :: ClientInfo
defaultClientInfo = makeClientInfo "Telos" "0.1.0"

connectToServer :: ServerConfig -> IO (Either MCPError MCPConnection)
connectToServer config = do
  handleResult <- spawnMCPProcess
    (config ^. scCommand)
    (config ^. scArgs)
    (config ^. scWorkDir)
    (config ^. scEnv)

  case handleResult of
    Left err     -> pure $ Left err
    Right handle -> do
      nextIdVar <- newTVarIO 1
      pendingReqs <- newTVarIO Map.empty

      let initParams
            = makeInitializeParams defaultClientInfo & ipCapabilities .~ defaultClientCapabilities

      initResult <- sendRequestDirect handle nextIdVar "initialize" (Just $ toJSON initParams)

      case initResult of
        Left err       -> do
          closeHandle handle
          pure $ Left err
        Right initResp -> do
          _ <- sendNotificationRaw handle "notifications/initialized" Nothing

          dispatcherTid <- forkIO $ messageDispatcher handle pendingReqs

          pure
            $ Right
              MCPConnection { _mcName = config ^. scName
                            , _mcHandle = handle
                            , _mcNextId = nextIdVar
                            , _mcCapabilities = initResp ^. irCapabilities
                            , _mcServerInfo = initResp ^. irServerInfo
                            , _mcPendingReqs = pendingReqs
                            , _mcDispatcherThread = dispatcherTid
                            }

messageDispatcher
  :: StdIOHandle -> TVar (Map Int (TMVar (Either MCPError JsonRpcResponse))) -> IO ()
messageDispatcher handle pendingReqs = forever $ do
  result <- try $ receiveRawMessage handle
  case result of
    Left (e :: SomeException) -> do
      pending <- STM.atomically $ do
        reqs <- STM.readTVar pendingReqs
        STM.writeTVar pendingReqs Map.empty
        pure reqs
      let err = Left $ MCPConnectionFailed $ T.pack $ show e
      mapM_ (\tmvar -> STM.atomically $ STM.tryPutTMVar tmvar err) (Map.elems pending)

    Right msgResult           -> case msgResult of
      Left _err -> pass
      Right msg -> handleMessage handle pendingReqs msg

handleMessage :: StdIOHandle
              -> TVar (Map Int (TMVar (Either MCPError JsonRpcResponse)))
              -> JsonRpcMessage
              -> IO ()
handleMessage handle pendingReqs msg = case msg of
  MsgResponse resp      -> case resp ^. jrsId of
    Just (IntId reqId) -> do
      mTmvar <- STM.atomically $ do
        reqs <- STM.readTVar pendingReqs
        case Map.lookup reqId reqs of
          Nothing    -> pure Nothing
          Just tmvar -> do
            STM.writeTVar pendingReqs (Map.delete reqId reqs)
            pure $ Just tmvar
      case mTmvar of
        Nothing    -> pass
        Just tmvar -> STM.atomically $ STM.putTMVar tmvar (Right resp)
    _ -> pass

  MsgRequest req        -> do
    response <- handleServerRequest req
    _ <- sendRawBytes handle response
    pure ()

  MsgNotification notif -> handleServerNotification notif

handleServerRequest :: JsonRpcRequest -> IO BL.ByteString
handleServerRequest req = case req ^. jrqMethod of
  "roots/list" -> do
    let result = toJSON $ object' [ ( "roots", toJSON ([] :: [ Value ]) ) ]
    pure $ encodeResponse (req ^. jrqId) (Right result)

  "sampling/createMessage" -> do
    let err = makeJsonRpcError (-32601) "Method not supported: sampling/createMessage"
    pure $ encodeResponse (req ^. jrqId) (Left err)

  _ -> do
    let err = makeJsonRpcError (-32601) ("Method not found: " <> (req ^. jrqMethod))
    pure $ encodeResponse (req ^. jrqId) (Left err)
  where
    object' = toJSON . Map.fromList :: [ ( Text, Value ) ] -> Value

handleServerNotification :: JsonRpcNotification -> IO ()
handleServerNotification notif = case notif ^. jrnMethod of
  "notifications/cancelled" -> pass
  "notifications/progress" -> pass
  "notifications/resources/list_changed" -> pass
  "notifications/tools/list_changed" -> pass
  "notifications/prompts/list_changed" -> pass
  _ -> pass

disconnectFromServer :: MCPConnection -> IO ()
disconnectFromServer conn = do
  killThread (conn ^. mcDispatcherThread)
  closeHandle (conn ^. mcHandle)

sendRequestDirect
  :: (FromJSON a) => StdIOHandle -> TVar Int -> Text -> Maybe Value -> IO (Either MCPError a)
sendRequestDirect handle nextIdVar method params = do
  reqId <- atomically $ do
    i <- readTVar nextIdVar
    writeTVar nextIdVar (i + 1)
    pure i

  let request = makeJsonRpcRequest (IntId reqId) method params

  sendResult <- sendMessage handle request
  case sendResult of
    Left err -> pure $ Left err
    Right () -> waitForResponseDirect handle reqId

waitForResponseDirect :: (FromJSON a) => StdIOHandle -> Int -> IO (Either MCPError a)
waitForResponseDirect handle expectedId = do
  msgResult <- receiveRawMessage handle
  case msgResult of
    Left err  -> pure $ Left $ MCPProtocolError (-32603) $ T.pack err
    Right msg -> case msg of
      MsgResponse resp  -> case resp ^. jrsId of
        Just (IntId respId)
          | respId == expectedId -> parseResponse resp
        _ -> waitForResponseDirect handle expectedId
      MsgRequest req    -> do
        response <- handleServerRequest req
        _ <- sendRawBytes handle response
        waitForResponseDirect handle expectedId
      MsgNotification _ -> waitForResponseDirect handle expectedId

sendRequestRaw :: (FromJSON a) => MCPConnection -> Text -> Maybe Value -> IO (Either MCPError a)
sendRequestRaw conn method params = do
  reqId <- atomically $ do
    i <- readTVar (conn ^. mcNextId)
    writeTVar (conn ^. mcNextId) (i + 1)
    pure i

  respTmvar <- newEmptyTMVarIO
  atomically $ modifyTVar' (conn ^. mcPendingReqs) (Map.insert reqId respTmvar)

  let request = makeJsonRpcRequest (IntId reqId) method params

  sendResult <- sendMessage (conn ^. mcHandle) request
  case sendResult of
    Left err -> do
      atomically $ modifyTVar' (conn ^. mcPendingReqs) (Map.delete reqId)
      pure $ Left err
    Right () -> do
      respResult <- atomically $ takeTMVar respTmvar
      case respResult of
        Left err   -> pure $ Left err
        Right resp -> parseResponse resp

sendNotificationRaw :: StdIOHandle -> Text -> Maybe Value -> IO (Either MCPError ())
sendNotificationRaw handle method params = do
  let notif = makeJsonRpcNotification method params
  sendMessage handle notif

parseResponse :: (FromJSON a) => JsonRpcResponse -> IO (Either MCPError a)
parseResponse resp = case resp ^. jrsError of
  Just err -> pure $ Left $ MCPProtocolError (err ^. jreCode) (err ^. jreMessage)
  Nothing  -> case resp ^. jrsResult of
    Nothing  -> pure $ Left $ MCPProtocolError (-32603) "Missing result in response"
    Just val -> case fromJSON val of
      Error str
        -> pure $ Left $ MCPProtocolError (-32603) $ T.pack $ "Failed to parse result: " <> str
      Success a -> pure $ Right a

sendRequest :: ( ToJSON params, FromJSON result )
            => MCPConnection
            -> Text
            -> params
            -> IO (Either MCPError result)
sendRequest conn method params = sendRequestRaw conn method (Just $ toJSON params)

sendNotification :: (ToJSON params) => MCPConnection -> Text -> params -> IO (Either MCPError ())
sendNotification conn method params
  = sendNotificationRaw (conn ^. mcHandle) method (Just $ toJSON params)

listTools :: MCPConnection -> IO (Either MCPError ListToolsResult)
listTools conn = sendRequestRaw conn "tools/list" Nothing

callTool :: MCPConnection -> Text -> Maybe Value -> IO (Either MCPError CallToolResult)
callTool conn tName arguments = do
  let params = makeCallToolParams tName arguments
  sendRequest conn "tools/call" params

listResources :: MCPConnection -> IO (Either MCPError ListResourcesResult)
listResources conn = sendRequestRaw conn "resources/list" Nothing

readResource :: MCPConnection -> Text -> IO (Either MCPError ReadResourceResult)
readResource conn uri = do
  let params = makeReadResourceParams uri
  sendRequest conn "resources/read" params
