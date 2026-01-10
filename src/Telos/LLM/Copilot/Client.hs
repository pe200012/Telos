{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.LLM.Copilot.Client
  ( CopilotClient(..)
  , clAuth
  , clManager
  , clConfig
  , CopilotConfig(..)
  , ccModel
  , ccMaxTokens
  , ChatRequest(..)
  , crModel
  , crMessages
  , crTools
  , crStream
  , crMaxTokens
  , crTemperature
  , ChatResponse(..)
  , chId
  , chObject
  , chCreated
  , chModel
  , chChoices
  , Choice(..)
  , chIndex
  , chMessage
  , chDelta
  , chFinishReason
  , Delta(..)
  , dRole
  , dContent
  , dToolCalls
  , ToolCallChunk(..)
  , tccIndex
  , tccId
  , tccType
  , tccFunction
  , FunctionChunk(..)
  , fcName
  , fcArguments
  , ModelInfo(..)
  , miId
  , miName
  , miVersion
  , ModelsResponse(..)
  , mrData
  , newCopilotClient
  , sendChatRequest
  , sendChatRequestStream
  , listModels
  ) where

import           Conduit

import           Control.Exception           ( try )

import           Data.Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import           Lens.Micro                  ( (^.), Lens', non )
import           Lens.Micro.TH               ( makeLenses )

import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit ( bodyReaderSource )
import           Network.HTTP.Types.Header   ( RequestHeaders )
import           Network.HTTP.Types.Status   ( statusCode )

import           Telos.Core.Types            ( AssistantMessage, Message, Tool )
import           Telos.LLM.Copilot.Auth      ( CopilotAuth, CopilotToken(..), ensureValidToken )

-- | Copilot client configuration
data CopilotConfig = CopilotConfig { _ccModel :: Text, _ccMaxTokens :: Maybe Int }
  deriving stock ( Show )

makeLenses ''CopilotConfig

-- | Unused but kept for API completeness
_defaultConfig :: CopilotConfig
_defaultConfig = CopilotConfig { _ccModel = "gpt-4.1", _ccMaxTokens = Nothing }

-- | Copilot API client
data CopilotClient
  = CopilotClient { _clAuth :: CopilotAuth, _clManager :: Manager, _clConfig :: CopilotConfig }

makeLenses ''CopilotClient

-- | Create a new Copilot client
newCopilotClient :: CopilotAuth -> Manager -> CopilotConfig -> CopilotClient
newCopilotClient auth mgr config
  = CopilotClient { _clAuth = auth, _clManager = mgr, _clConfig = config }

-- | Chat completion request (OpenAI compatible)
data ChatRequest
  = ChatRequest { _crModel       :: Text
                , _crMessages    :: [ Message ]
                , _crTools       :: Maybe [ Tool ]
                , _crStream      :: Bool
                , _crMaxTokens   :: Maybe Int
                , _crTemperature :: Maybe Double
                }
  deriving stock ( Show, Generic )

makeLenses ''ChatRequest

instance ToJSON ChatRequest where
  toJSON req
    = object
    $ filter
      ((/= Null) . snd)
      [ "model" .= (req ^. crModel)
      , "messages" .= (req ^. crMessages)
      , "tools" .= fmap (map wrapTool) (req ^. crTools)
      , "stream" .= (req ^. crStream)
      , "max_tokens" .= (req ^. crMaxTokens)
      , "temperature" .= (req ^. crTemperature)
      ]
    where
      wrapTool tool = object [ "type" .= ("function" :: Text), "function" .= tool ]

-- | Function chunk for streaming tool calls (must be defined first)
data FunctionChunk = FunctionChunk { _fcName :: Maybe Text, _fcArguments :: Maybe Text }
  deriving stock ( Show, Generic )

makeLenses ''FunctionChunk

instance FromJSON FunctionChunk where
  parseJSON
    = withObject "FunctionChunk" $ \o -> FunctionChunk <$> o .:? "name" <*> o .:? "arguments"

-- | Tool call chunk for streaming (must be before Delta)
data ToolCallChunk
  = ToolCallChunk { _tccIndex    :: Int
                  , _tccId       :: Maybe Text
                  , _tccType     :: Maybe Text
                  , _tccFunction :: Maybe FunctionChunk
                  }
  deriving stock ( Show, Generic )

makeLenses ''ToolCallChunk

instance FromJSON ToolCallChunk where
  parseJSON = withObject "ToolCallChunk" $ \o
    -> ToolCallChunk <$> o .: "index" <*> o .:? "id" <*> o .:? "type" <*> o .:? "function"

-- | Streaming delta (must be before Choice)
data Delta
  = Delta { _dRole :: Maybe Text, _dContent :: Maybe Text, _dToolCalls :: Maybe [ ToolCallChunk ] }
  deriving stock ( Show, Generic )

makeLenses ''Delta

instance FromJSON Delta where
  parseJSON
    = withObject "Delta" $ \o -> Delta <$> o .:? "role" <*> o .:? "content" <*> o .:? "tool_calls"

-- | Response choice (now Delta is in scope)
data Choice
  = Choice { _chIndex        :: Int
           , _chMessage      :: Maybe AssistantMessage  -- For non-streaming
           , _chDelta        :: Maybe Delta             -- For streaming
           , _chFinishReason :: Maybe Text
           }
  deriving stock ( Show, Generic )

makeLenses ''Choice

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o
    -> Choice <$> o .: "index" <*> o .:? "message" <*> o .:? "delta" <*> o .:? "finish_reason"

-- | Chat completion response (after Choice is defined)
data ChatResponse
  = ChatResponse { _chId      :: Text
                 , _chObject  :: Maybe Text  -- Optional, not all providers return this
                 , _chCreated :: Maybe Int  -- Optional
                 , _chModel   :: Text
                 , _chChoices :: [ Choice ]
                 }
  deriving stock ( Show, Generic )

makeLenses ''ChatResponse

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \o -> ChatResponse <$> o .: "id"
    <*> o .:? "object"
    <*> o .:? "created"
    <*> o .: "model"
    <*> o .: "choices"

-- | Copilot-specific headers
copilotHeaders :: CopilotToken -> RequestHeaders
copilotHeaders token
  = [ ( "Authorization", "Bearer " <> TE.encodeUtf8 (token ^. ctToken') )
    , ( "Content-Type", "application/json" )
    , ( "Accept", "application/json" )
    , ( "copilot-integration-id", "vscode-chat" )
    , ( "editor-version", "vscode/1.95.0" )
    , ( "editor-plugin-version", "copilot-chat/0.26.7" )
    , ( "User-Agent", "GitHubCopilotChat/0.26.7" )
    , ( "x-github-api-version", "2025-04-01" )
    , ( "X-Initiator", "user" )
    ]
  where
    -- Temporary accessor until Auth is refactored
    ctToken' :: Lens' CopilotToken Text
    ctToken' f (CopilotToken t e) = (`CopilotToken` e) <$> f t

-- | Build chat request
buildRequest :: CopilotClient -> CopilotToken -> [ Message ] -> [ Tool ] -> Bool -> Request
buildRequest client token messages tools stream
  = let
      config  = client ^. clConfig
      chatReq
        = ChatRequest
        { _crModel       = config ^. ccModel
        , _crMessages    = messages
        , _crTools       = if null tools
            then Nothing
            else Just tools
        , _crStream      = stream
        , _crMaxTokens   = config ^. ccMaxTokens
        , _crTemperature = Nothing
        }
      body    = encode chatReq
    in 
      defaultRequest
      { host           = "api.githubcopilot.com"
      , port           = 443
      , path           = "/chat/completions"
      , method         = "POST"
      , secure         = True
      , requestHeaders = copilotHeaders token
      , requestBody    = RequestBodyLBS body
      }

-- | Send non-streaming chat request
sendChatRequest :: CopilotClient -> [ Message ] -> [ Tool ] -> IO (Either Text ChatResponse)
sendChatRequest client messages tools = do
  tokenResult <- ensureValidToken (client ^. clAuth)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      let req = buildRequest client token messages tools False
      result <- try $ httpLbs req (client ^. clManager)
      case result of
        Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
        Right resp -> case statusCode (responseStatus resp) of
          200  -> case eitherDecode (responseBody resp) of
            Left err       -> pure $ Left $ T.pack err
            Right chatResp -> pure $ Right chatResp
          code -> pure
            $ Left
            $ "HTTP "
            <> T.pack (show code)
            <> ": "
            <> TE.decodeUtf8 (BL.toStrict (responseBody resp))

-- | Send streaming chat request, returns Conduit source
sendChatRequestStream
  :: CopilotClient -> [ Message ] -> [ Tool ] -> IO (Either Text (ConduitT () ChatResponse IO ()))
sendChatRequestStream client messages tools = do
  tokenResult <- ensureValidToken (client ^. clAuth)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      let req
            = (buildRequest client token messages tools True)
            { requestHeaders = copilotHeaders token ++ [ ( "Accept", "text/event-stream" ) ] }

      -- Create streaming response
      result <- try $ responseOpen req (client ^. clManager)
      case result of
        Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
        Right resp -> case statusCode (responseStatus resp) of
          200
            -> pure $ Right $ parseSSESource (bodyReaderSource $ responseBody resp) .| parseChunks
          code -> do
            body <- brConsume (responseBody resp)
            responseClose resp
            pure $ Left $ "HTTP " <> T.pack (show code) <> ": " <> TE.decodeUtf8 (BS.concat body)

-- | Parse SSE events from raw bytes
parseSSESource :: ConduitT () BS.ByteString IO () -> ConduitT () BS.ByteString IO ()
parseSSESource source = source .| linesC .| filterC isDataLine .| mapC extractData
  where
    isDataLine bs = "data: " `BS.isPrefixOf` bs

    extractData   = BS.drop 6  -- Remove "data: " prefix

-- | Split input into lines
linesC :: ConduitT BS.ByteString BS.ByteString IO ()
linesC = awaitForever $ \chunk -> do
  let ( lines', _rest ) = splitLines chunk
  mapM_ yield lines'
  where
    splitLines bs
      = let
          parts = BS8.split '\n' bs
        in 
          case parts of
            []    -> ( [], BS.empty )
            [ x ] -> ( [], x )
            xs    -> ( maybe [] toList (viaNonEmpty init xs), viaNonEmpty last xs ^. non BS.empty )

-- | Parse JSON chunks from SSE data
parseChunks :: ConduitT BS.ByteString ChatResponse IO ()
parseChunks
  = awaitForever $ \chunk -> unless (chunk == "[DONE]") $ case eitherDecodeStrict chunk of
    Left _     -> pass  -- Skip parse errors
    Right resp -> yield resp

-- | Model information
data ModelInfo = ModelInfo { _miId :: Text, _miName :: Maybe Text, _miVersion :: Maybe Text }
  deriving stock ( Show, Generic )

makeLenses ''ModelInfo

instance FromJSON ModelInfo where
  parseJSON
    = withObject "ModelInfo" $ \o -> ModelInfo <$> o .: "id" <*> o .:? "name" <*> o .:? "version"

-- | Models list response
newtype ModelsResponse = ModelsResponse { _mrData :: [ ModelInfo ] }
  deriving stock ( Show, Generic )

makeLenses ''ModelsResponse

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \o -> ModelsResponse <$> o .: "data"

-- | List available models
listModels :: CopilotClient -> IO (Either Text ModelsResponse)
listModels client = do
  tokenResult <- ensureValidToken (client ^. clAuth)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      initReq <- parseRequest "https://api.githubcopilot.com/models"
      let req = initReq { requestHeaders = copilotHeaders token }

      result <- try $ httpLbs req (client ^. clManager)
      case result of
        Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
        Right resp -> case statusCode (responseStatus resp) of
          200  -> case eitherDecode (responseBody resp) of
            Left err     -> pure $ Left $ T.pack err
            Right models -> pure $ Right models
          code -> pure
            $ Left
            $ "HTTP "
            <> T.pack (show code)
            <> ": "
            <> TE.decodeUtf8 (BL.toStrict (responseBody resp))
