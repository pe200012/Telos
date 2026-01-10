module Telos.MCP.JsonRpc
  ( JsonRpcRequest(..)
  , JsonRpcResponse(..)
  , JsonRpcError(..)
  , JsonRpcNotification(..)
  , JsonRpcMessage(..)
  , RequestId(..)
  , encodeRequest
  , encodeNotification
  , encodeResponse
  , decodeResponse
  , decodeMessage
  , standardErrorCodes
  ) where

import           Data.Aeson
import           Data.Aeson.Types          ( Parser )
import qualified Data.ByteString.Lazy      as BL

data RequestId = IntId Int | TextId Text
  deriving stock ( Eq, Show, Generic )

instance ToJSON RequestId where
  toJSON (IntId n)  = toJSON n
  toJSON (TextId t) = toJSON t

instance FromJSON RequestId where
  parseJSON (Number n) = pure $ IntId (round n)
  parseJSON (String t) = pure $ TextId t
  parseJSON _          = fail "RequestId must be number or string"

data JsonRpcRequest
  = JsonRpcRequest { jrqId :: RequestId, jrqMethod :: Text, jrqParams :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

instance ToJSON JsonRpcRequest where
  toJSON req
    = object
    $ [ "jsonrpc" .= ("2.0" :: Text), "id" .= jrqId req, "method" .= jrqMethod req ]
    ++ maybe [] (\p -> [ "params" .= p ]) (jrqParams req)

data JsonRpcNotification = JsonRpcNotification { jrnMethod :: Text, jrnParams :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

instance ToJSON JsonRpcNotification where
  toJSON notif
    = object
    $ [ "jsonrpc" .= ("2.0" :: Text), "method" .= jrnMethod notif ]
    ++ maybe [] (\p -> [ "params" .= p ]) (jrnParams notif)

data JsonRpcError = JsonRpcError { jreCode :: Int, jreMessage :: Text, jreData :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o
    -> JsonRpcError <$> o .: "code" <*> o .: "message" <*> o .:? "data"

data JsonRpcResponse
  = JsonRpcResponse
  { jrsId :: Maybe RequestId, jrsResult :: Maybe Value, jrsError :: Maybe JsonRpcError }
  deriving stock ( Eq, Show, Generic )

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \o -> do
    version <- o .: "jsonrpc" :: Parser Text
    if version /= "2.0"
      then fail $ "Unsupported JSON-RPC version: " <> show version
      else JsonRpcResponse <$> o .:? "id" <*> o .:? "result" <*> o .:? "error"

encodeRequest :: JsonRpcRequest -> BL.ByteString
encodeRequest = encode

encodeNotification :: JsonRpcNotification -> BL.ByteString
encodeNotification = encode

decodeResponse :: BL.ByteString -> Either String JsonRpcResponse
decodeResponse = eitherDecode

standardErrorCodes :: [ ( Int, Text ) ]
standardErrorCodes
  = [ ( -32700, "Parse error" )
    , ( -32600, "Invalid Request" )
    , ( -32601, "Method not found" )
    , ( -32602, "Invalid params" )
    , ( -32603, "Internal error" )
    ]

-- | Unified message type for incoming JSON-RPC messages
data JsonRpcMessage
  = MsgRequest JsonRpcRequest      -- Request from server (has id + method)
  | MsgResponse JsonRpcResponse    -- Response to our request (has id + result/error)
  | MsgNotification JsonRpcNotification  -- Notification from server (has method, no id)
  deriving stock ( Eq, Show )

instance FromJSON JsonRpcMessage where
  parseJSON = withObject "JsonRpcMessage" $ \o -> do
    version <- o .: "jsonrpc" :: Parser Text
    if version /= "2.0"
      then fail $ "Unsupported JSON-RPC version: " <> show version
      else do
        mId <- o .:? "id" :: Parser (Maybe RequestId)
        mMethod <- o .:? "method" :: Parser (Maybe Text)
        mResult <- o .:? "result" :: Parser (Maybe Value)
        mError <- o .:? "error" :: Parser (Maybe JsonRpcError)
        mParams <- o .:? "params" :: Parser (Maybe Value)

        case ( mId, mMethod, mResult, mError ) of
          -- Response: has id and (result or error), no method
          ( Just rid, Nothing, _, _ )
            -> pure $ MsgResponse $ JsonRpcResponse (Just rid) mResult mError
          -- Request: has id and method
          ( Just rid, Just method, _, _ ) -> pure $ MsgRequest $ JsonRpcRequest rid method mParams
          -- Notification: has method but no id
          ( Nothing, Just method, _, _ )
            -> pure $ MsgNotification $ JsonRpcNotification method mParams
          -- Invalid
          _ -> fail "Invalid JSON-RPC message: must have method or result/error"

-- | Parse incoming request (for handling server requests)
instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> do
    version <- o .: "jsonrpc" :: Parser Text
    if version /= "2.0"
      then fail $ "Unsupported JSON-RPC version: " <> show version
      else JsonRpcRequest <$> o .: "id" <*> o .: "method" <*> o .:? "params"

encodeResponse :: RequestId -> Either JsonRpcError Value -> BL.ByteString
encodeResponse rid (Left err)
  = encode
  $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= rid
    , "error" .= object [ "code" .= jreCode err, "message" .= jreMessage err ]
    ]
encodeResponse rid (Right result)
  = encode $ object [ "jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result ]

decodeMessage :: BL.ByteString -> Either String JsonRpcMessage
decodeMessage = eitherDecode
