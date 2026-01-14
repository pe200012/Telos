{-# LANGUAGE TemplateHaskell #-}

module Telos.MCP.JsonRpc
  ( JsonRpcRequest
  , makeJsonRpcRequest
  , jrqId
  , jrqMethod
  , jrqParams
  , JsonRpcResponse
  , jrsId
  , jrsResult
  , jrsError
  , JsonRpcError
  , makeJsonRpcError
  , jreCode
  , jreMessage
  , jreData
  , JsonRpcNotification
  , makeJsonRpcNotification
  , jrnMethod
  , jrnParams
  , JsonRpcMessage(..)
  , RequestId(..)
  , encodeRequest
  , encodeNotification
  , encodeResponse
  , decodeResponse
  , decodeMessage
  , standardErrorCodes
  ) where

import           Control.Lens         ( (^.), makeLenses )

import           Data.Aeson
import           Data.Aeson.Types     ( Parser )
import qualified Data.ByteString.Lazy as BL

import           Relude

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
  = JsonRpcRequest { _jrqId :: RequestId, _jrqMethod :: Text, _jrqParams :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

makeLenses ''JsonRpcRequest

makeJsonRpcRequest :: RequestId -> Text -> Maybe Value -> JsonRpcRequest
makeJsonRpcRequest rid method params
  = JsonRpcRequest { _jrqId = rid, _jrqMethod = method, _jrqParams = params }

instance ToJSON JsonRpcRequest where
  toJSON req
    = object
    $ [ "jsonrpc" .= ("2.0" :: Text), "id" .= (req ^. jrqId), "method" .= (req ^. jrqMethod) ]
    ++ maybe [] (\p -> [ "params" .= p ]) (req ^. jrqParams)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> do
    version <- o .: "jsonrpc" :: Parser Text
    if version /= "2.0"
      then fail $ "Unsupported JSON-RPC version: " <> show version
      else JsonRpcRequest <$> o .: "id" <*> o .: "method" <*> o .:? "params"

data JsonRpcNotification = JsonRpcNotification { _jrnMethod :: Text, _jrnParams :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

makeLenses ''JsonRpcNotification

makeJsonRpcNotification :: Text -> Maybe Value -> JsonRpcNotification
makeJsonRpcNotification method params
  = JsonRpcNotification { _jrnMethod = method, _jrnParams = params }

instance ToJSON JsonRpcNotification where
  toJSON notif
    = object
    $ [ "jsonrpc" .= ("2.0" :: Text), "method" .= (notif ^. jrnMethod) ]
    ++ maybe [] (\p -> [ "params" .= p ]) (notif ^. jrnParams)

data JsonRpcError = JsonRpcError { _jreCode :: Int, _jreMessage :: Text, _jreData :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

makeLenses ''JsonRpcError

makeJsonRpcError :: Int -> Text -> JsonRpcError
makeJsonRpcError code message
  = JsonRpcError { _jreCode = code, _jreMessage = message, _jreData = Nothing }

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o
    -> JsonRpcError <$> o .: "code" <*> o .: "message" <*> o .:? "data"

data JsonRpcResponse
  = JsonRpcResponse
  { _jrsId :: Maybe RequestId, _jrsResult :: Maybe Value, _jrsError :: Maybe JsonRpcError }
  deriving stock ( Eq, Show, Generic )

makeLenses ''JsonRpcResponse

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

data JsonRpcMessage
  = MsgRequest JsonRpcRequest
  | MsgResponse JsonRpcResponse
  | MsgNotification JsonRpcNotification
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
          ( Just rid, Nothing, _, _ )
            -> pure $ MsgResponse $ JsonRpcResponse (Just rid) mResult mError
          ( Just rid, Just method, _, _ )
            -> pure $ MsgRequest $ makeJsonRpcRequest rid method mParams
          ( Nothing, Just method, _, _ )
            -> pure $ MsgNotification $ makeJsonRpcNotification method mParams
          _ -> fail "Invalid JSON-RPC message: must have method or result/error"

encodeResponse :: RequestId -> Either JsonRpcError Value -> BL.ByteString
encodeResponse rid (Left err)
  = encode
  $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= rid
    , "error" .= object [ "code" .= (err ^. jreCode), "message" .= (err ^. jreMessage) ]
    ]
encodeResponse rid (Right result)
  = encode $ object [ "jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result ]

decodeMessage :: BL.ByteString -> Either String JsonRpcMessage
decodeMessage = eitherDecode
