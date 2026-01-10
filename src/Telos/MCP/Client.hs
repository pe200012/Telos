module Telos.MCP.Client
  ( MCPConnection(..)
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
import           Control.Concurrent.STM
import           Control.Exception         ( SomeException, catch, try )
import           Control.Monad             ( forever, void )

import           Data.Aeson                ( FromJSON
                                           , Result(..)
                                           , ToJSON
                                           , Value
                                           , encode
                                           , fromJSON
                                           , toJSON
                                           )
import qualified Data.ByteString.Lazy      as BL
import           Data.Map.Strict           ( Map )
import qualified Data.Map.Strict           as Map
import           Data.Text                 ( Text )
import qualified Data.Text                 as T

import           Telos.Core.Error          ( MCPError(..) )
import           Telos.MCP.JsonRpc
import           Telos.MCP.Transport.StdIO
import           Telos.MCP.Types

-- | MCP Connection with message dispatcher
data MCPConnection
  = MCPConnection
  { mcName :: Text
  , mcHandle :: StdIOHandle
  , mcNextId :: TVar Int
  , mcCapabilities :: ServerCapabilities
  , mcServerInfo :: Maybe ServerInfo
  , mcPendingReqs :: TVar (Map Int (TMVar (Either MCPError JsonRpcResponse)))
  , mcDispatcherThread :: ThreadId
  }

-- | Client capabilities we advertise to the server
defaultClientCapabilities :: ClientCapabilities
defaultClientCapabilities
  = ClientCapabilities
  { ccRoots = Just RootsCapability { rcListChanged = Just True }, ccSampling = Nothing }

defaultClientInfo :: ClientInfo
defaultClientInfo = ClientInfo { ciName = "Telos", ciVersion = "0.1.0" }

-- | Connect to an MCP server
connectToServer :: ServerConfig -> IO (Either MCPError MCPConnection)
connectToServer config = do
  handleResult
    <- spawnMCPProcess (scCommand config) (scArgs config) (scWorkDir config) (scEnv config)

  case handleResult of
    Left err     -> pure $ Left err
    Right handle -> do
      nextIdVar <- newTVarIO 1
      pendingReqs <- newTVarIO Map.empty

      -- Perform initialization BEFORE starting dispatcher
      -- (so we can do synchronous request/response)
      let initParams
            = InitializeParams { ipProtocolVersion = currentProtocolVersion
                               , ipCapabilities    = defaultClientCapabilities
                               , ipClientInfo      = defaultClientInfo
                               }

      initResult <- sendRequestDirect handle nextIdVar "initialize" (Just $ toJSON initParams)

      case initResult of
        Left err       -> do
          closeHandle handle
          pure $ Left err
        Right initResp -> do
          -- Send initialized notification
          _ <- sendNotificationRaw handle "notifications/initialized" Nothing

          -- Start message dispatcher thread
          dispatcherTid <- forkIO $ messageDispatcher handle pendingReqs

          pure
            $ Right
              MCPConnection { mcName = scName config
                            , mcHandle = handle
                            , mcNextId = nextIdVar
                            , mcCapabilities = irCapabilities initResp
                            , mcServerInfo = irServerInfo initResp
                            , mcPendingReqs = pendingReqs
                            , mcDispatcherThread = dispatcherTid
                            }

-- | Message dispatcher - runs in background thread
messageDispatcher
  :: StdIOHandle -> TVar (Map Int (TMVar (Either MCPError JsonRpcResponse))) -> IO ()
messageDispatcher handle pendingReqs = forever $ do
  result <- try $ receiveRawMessage handle
  case result of
    Left (e :: SomeException) -> do
      -- Connection closed or error - notify all pending requests
      pending <- atomically $ do
        reqs <- readTVar pendingReqs
        writeTVar pendingReqs Map.empty
        pure reqs
      let err = Left $ MCPConnectionFailed $ T.pack $ show e
      mapM_ (\tmvar -> atomically $ tryPutTMVar tmvar err) (Map.elems pending)

    Right msgResult           -> case msgResult of
      Left err  -> pure ()  -- Parse error, skip
      Right msg -> handleMessage handle pendingReqs msg

-- | Handle an incoming message
handleMessage :: StdIOHandle
              -> TVar (Map Int (TMVar (Either MCPError JsonRpcResponse)))
              -> JsonRpcMessage
              -> IO ()
handleMessage handle pendingReqs msg = case msg of
  -- Response to our request
  MsgResponse resp      -> case jrsId resp of
    Just (IntId reqId) -> do
      mTmvar <- atomically $ do
        reqs <- readTVar pendingReqs
        case Map.lookup reqId reqs of
          Nothing    -> pure Nothing
          Just tmvar -> do
            writeTVar pendingReqs (Map.delete reqId reqs)
            pure $ Just tmvar
      case mTmvar of
        Nothing    -> pure ()  -- No one waiting for this response
        Just tmvar -> atomically $ putTMVar tmvar (Right resp)
    _ -> pure ()  -- Unexpected ID type

  -- Request from server - we need to respond
  MsgRequest req        -> do
    response <- handleServerRequest req
    _ <- sendRawBytes handle response
    pure ()

  -- Notification from server - no response needed
  MsgNotification notif -> handleServerNotification notif

-- | Handle requests from the server
handleServerRequest :: JsonRpcRequest -> IO BL.ByteString
handleServerRequest req = case jrqMethod req of
  "roots/list" -> do
    -- Return empty roots list for now
    -- In future, this could be configurable
    let result = toJSON $ object' [ ( "roots", toJSON ([] :: [ Value ]) ) ]
    pure $ encodeResponse (jrqId req) (Right result)

  "sampling/createMessage" -> do
    -- We don't support sampling yet
    let err = JsonRpcError (-32601) "Method not supported: sampling/createMessage" Nothing
    pure $ encodeResponse (jrqId req) (Left err)

  _ -> do
    -- Unknown method
    let err = JsonRpcError (-32601) ("Method not found: " <> jrqMethod req) Nothing
    pure $ encodeResponse (jrqId req) (Left err)
  where
    object' = toJSON . Map.fromList :: [ ( Text, Value ) ] -> Value

-- | Handle notifications from the server
handleServerNotification :: JsonRpcNotification -> IO ()
handleServerNotification notif = case jrnMethod notif of
  "notifications/cancelled" -> pure ()  -- Request was cancelled
  "notifications/progress" -> pure ()   -- Progress update
  "notifications/resources/list_changed" -> pure ()  -- Resources changed
  "notifications/tools/list_changed" -> pure ()  -- Tools changed
  "notifications/prompts/list_changed" -> pure ()  -- Prompts changed
  _ -> pure ()  -- Unknown notification, ignore

-- | Disconnect from server
disconnectFromServer :: MCPConnection -> IO ()
disconnectFromServer conn = do
  killThread (mcDispatcherThread conn)
  closeHandle (mcHandle conn)

-- | Send request directly (before dispatcher is running)
sendRequestDirect
  :: (FromJSON a) => StdIOHandle -> TVar Int -> Text -> Maybe Value -> IO (Either MCPError a)
sendRequestDirect handle nextIdVar method params = do
  reqId <- atomically $ do
    i <- readTVar nextIdVar
    writeTVar nextIdVar (i + 1)
    pure i

  let request = JsonRpcRequest { jrqId = IntId reqId, jrqMethod = method, jrqParams = params }

  sendResult <- sendMessage handle request
  case sendResult of
    Left err -> pure $ Left err
    Right () -> waitForResponseDirect handle reqId

-- | Wait for response directly (skipping other messages, before dispatcher)
waitForResponseDirect :: (FromJSON a) => StdIOHandle -> Int -> IO (Either MCPError a)
waitForResponseDirect handle expectedId = do
  msgResult <- receiveRawMessage handle
  case msgResult of
    Left err  -> pure $ Left $ MCPProtocolError (-32603) $ T.pack err
    Right msg -> case msg of
      MsgResponse resp  -> case jrsId resp of
        Just (IntId respId)
          | respId == expectedId -> parseResponse resp
        _ -> waitForResponseDirect handle expectedId  -- Wrong ID, keep waiting
      MsgRequest req    -> do
        -- Handle server request during init
        response <- handleServerRequest req
        _ <- sendRawBytes handle response
        waitForResponseDirect handle expectedId
      MsgNotification _ -> waitForResponseDirect handle expectedId

-- | Send request through dispatcher
sendRequestRaw :: (FromJSON a) => MCPConnection -> Text -> Maybe Value -> IO (Either MCPError a)
sendRequestRaw conn method params = do
  reqId <- atomically $ do
    i <- readTVar (mcNextId conn)
    writeTVar (mcNextId conn) (i + 1)
    pure i

  -- Create response channel
  respTmvar <- newEmptyTMVarIO
  atomically $ modifyTVar' (mcPendingReqs conn) (Map.insert reqId respTmvar)

  let request = JsonRpcRequest { jrqId = IntId reqId, jrqMethod = method, jrqParams = params }

  sendResult <- sendMessage (mcHandle conn) request
  case sendResult of
    Left err -> do
      atomically $ modifyTVar' (mcPendingReqs conn) (Map.delete reqId)
      pure $ Left err
    Right () -> do
      -- Wait for response from dispatcher
      respResult <- atomically $ takeTMVar respTmvar
      case respResult of
        Left err   -> pure $ Left err
        Right resp -> parseResponse resp

sendNotificationRaw :: StdIOHandle -> Text -> Maybe Value -> IO (Either MCPError ())
sendNotificationRaw handle method params = do
  let notif = JsonRpcNotification { jrnMethod = method, jrnParams = params }
  sendMessage handle notif

parseResponse :: (FromJSON a) => JsonRpcResponse -> IO (Either MCPError a)
parseResponse resp = case jrsError resp of
  Just err -> pure $ Left $ MCPProtocolError (jreCode err) (jreMessage err)
  Nothing  -> case jrsResult resp of
    Nothing  -> pure $ Left $ MCPProtocolError (-32603) "Missing result in response"
    Just val -> case fromJSON val of
      Error str
        -> pure $ Left $ MCPProtocolError (-32603) $ T.pack $ "Failed to parse result: " <> str
      Success a -> pure $ Right a

-- Public API

sendRequest :: ( ToJSON params, FromJSON result )
            => MCPConnection
            -> Text
            -> params
            -> IO (Either MCPError result)
sendRequest conn method params = sendRequestRaw conn method (Just $ toJSON params)

sendNotification :: (ToJSON params) => MCPConnection -> Text -> params -> IO (Either MCPError ())
sendNotification conn method params
  = sendNotificationRaw (mcHandle conn) method (Just $ toJSON params)

listTools :: MCPConnection -> IO (Either MCPError ListToolsResult)
listTools conn = sendRequestRaw conn "tools/list" Nothing

callTool :: MCPConnection -> Text -> Maybe Value -> IO (Either MCPError CallToolResult)
callTool conn name arguments = do
  let params = CallToolParams { ctpName = name, ctpArguments = arguments }
  sendRequest conn "tools/call" params

listResources :: MCPConnection -> IO (Either MCPError ListResourcesResult)
listResources conn = sendRequestRaw conn "resources/list" Nothing

readResource :: MCPConnection -> Text -> IO (Either MCPError ReadResourceResult)
readResource conn uri = do
  let params = ReadResourceParams { rrpUri = uri }
  sendRequest conn "resources/read" params
