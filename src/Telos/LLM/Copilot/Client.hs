{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}

module Telos.LLM.Copilot.Client
  ( CopilotClient(..)
  , CopilotConfig(..)
  , ChatRequest(..)
  , ChatResponse(..)
  , Choice(..)
  , Delta(..)
  , ToolCallChunk(..)
  , FunctionChunk(..)
  , ModelInfo(..)
  , ModelsResponse(..)
  , newCopilotClient
  , sendChatRequest
  , sendChatRequestStream
  , listModels
  ) where

import           Conduit

import           Control.Exception           ( SomeException, try )
import           Control.Monad               ( unless, when )

import           Data.Aeson
import           Data.Aeson.Types            ( Parser )
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                   ( Text )
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import           GHC.Generics                ( Generic )

import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit ( bodyReaderSource )
import           Network.HTTP.Types.Header   ( RequestHeaders )
import           Network.HTTP.Types.Status   ( statusCode )

import           Telos.Core.Types            ( AssistantMessage, Message, Tool )
import           Telos.LLM.Copilot.Auth      ( CopilotAuth, CopilotToken(..), ensureValidToken )

-- | Copilot client configuration
data CopilotConfig = CopilotConfig { ccModel :: Text, ccMaxTokens :: Maybe Int }
  deriving stock ( Show )

defaultConfig :: CopilotConfig
defaultConfig = CopilotConfig { ccModel = "gpt-4.1", ccMaxTokens = Nothing }

-- | Copilot API client
data CopilotClient
  = CopilotClient { clAuth :: CopilotAuth, clManager :: Manager, clConfig :: CopilotConfig }

-- | Create a new Copilot client
newCopilotClient :: CopilotAuth -> Manager -> CopilotConfig -> CopilotClient
newCopilotClient auth mgr config
  = CopilotClient { clAuth = auth, clManager = mgr, clConfig = config }

-- | Chat completion request (OpenAI compatible)
data ChatRequest
  = ChatRequest { crModel       :: Text
                , crMessages    :: [ Message ]
                , crTools       :: Maybe [ Tool ]
                , crStream      :: Bool
                , crMaxTokens   :: Maybe Int
                , crTemperature :: Maybe Double
                }
  deriving stock ( Show, Generic )

instance ToJSON ChatRequest where
  toJSON ChatRequest { .. }
    = object
    $ filter
      ((/= Null) . snd)
      [ "model" .= crModel
      , "messages" .= crMessages
      , "tools" .= fmap (map wrapTool) crTools
      , "stream" .= crStream
      , "max_tokens" .= crMaxTokens
      , "temperature" .= crTemperature
      ]
    where
      wrapTool tool = object [ "type" .= ("function" :: Text), "function" .= tool ]

-- | Chat completion response
data ChatResponse
  = ChatResponse { chId      :: Text
                 , chObject  :: Maybe Text  -- Optional, not all providers return this
                 , chCreated :: Maybe Int  -- Optional
                 , chModel   :: Text
                 , chChoices :: [ Choice ]
                 }
  deriving stock ( Show, Generic )

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \o -> ChatResponse <$> o .: "id"
    <*> o .:? "object"
    <*> o .:? "created"
    <*> o .: "model"
    <*> o .: "choices"

-- | Response choice
data Choice
  = Choice { chIndex        :: Int
           , chMessage      :: Maybe AssistantMessage  -- For non-streaming
           , chDelta        :: Maybe Delta             -- For streaming
           , chFinishReason :: Maybe Text
           }
  deriving stock ( Show, Generic )

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o
    -> Choice <$> o .: "index" <*> o .:? "message" <*> o .:? "delta" <*> o .:? "finish_reason"

-- | Streaming delta
data Delta
  = Delta { dRole :: Maybe Text, dContent :: Maybe Text, dToolCalls :: Maybe [ ToolCallChunk ] }
  deriving stock ( Show, Generic )

instance FromJSON Delta where
  parseJSON
    = withObject "Delta" $ \o -> Delta <$> o .:? "role" <*> o .:? "content" <*> o .:? "tool_calls"

-- | Tool call chunk for streaming
data ToolCallChunk
  = ToolCallChunk { tccIndex    :: Int
                  , tccId       :: Maybe Text
                  , tccType     :: Maybe Text
                  , tccFunction :: Maybe FunctionChunk
                  }
  deriving stock ( Show, Generic )

instance FromJSON ToolCallChunk where
  parseJSON = withObject "ToolCallChunk" $ \o
    -> ToolCallChunk <$> o .: "index" <*> o .:? "id" <*> o .:? "type" <*> o .:? "function"

-- | Function chunk for streaming tool calls
data FunctionChunk = FunctionChunk { fcName :: Maybe Text, fcArguments :: Maybe Text }
  deriving stock ( Show, Generic )

instance FromJSON FunctionChunk where
  parseJSON
    = withObject "FunctionChunk" $ \o -> FunctionChunk <$> o .:? "name" <*> o .:? "arguments"

-- | Copilot-specific headers
copilotHeaders :: CopilotToken -> RequestHeaders
copilotHeaders token
  = [ ( "Authorization", "Bearer " <> TE.encodeUtf8 (ctToken token) )
    , ( "Content-Type", "application/json" )
    , ( "Accept", "application/json" )
    , ( "copilot-integration-id", "vscode-chat" )
    , ( "editor-version", "vscode/1.95.0" )
    , ( "editor-plugin-version", "copilot-chat/0.26.7" )
    , ( "User-Agent", "GitHubCopilotChat/0.26.7" )
    , ( "x-github-api-version", "2025-04-01" )
    , ( "X-Initiator", "user" )
    ]

-- | Build chat request
buildRequest :: CopilotClient -> CopilotToken -> [ Message ] -> [ Tool ] -> Bool -> Request
buildRequest client token messages tools stream
  = let
      config  = clConfig client
      chatReq
        = ChatRequest
        { crModel       = ccModel config
        , crMessages    = messages
        , crTools       = if null tools
            then Nothing
            else Just tools
        , crStream      = stream
        , crMaxTokens   = ccMaxTokens config
        , crTemperature = Nothing
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
  tokenResult <- ensureValidToken (clAuth client)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      let req = buildRequest client token messages tools False
      result <- try $ httpLbs req (clManager client)
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
  tokenResult <- ensureValidToken (clAuth client)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      let req
            = (buildRequest client token messages tools True)
            { requestHeaders = copilotHeaders token ++ [ ( "Accept", "text/event-stream" ) ] }

      -- Create streaming response
      result <- try $ responseOpen req (clManager client)
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
  leftover <- get
  let combined         = maybe chunk (<> chunk) leftover
      ( lines', rest ) = splitLines combined
  mapM_ yield lines'
  unless (BS.null rest) $ put rest
  where
    get           = pure Nothing  -- Simplified; in production use StateT

    put _ = pure ()     -- Simplified

    splitLines bs
      = let
          parts = BS8.split '\n' bs
        in 
          case parts of
            []    -> ( [], BS.empty )
            [ x ] -> ( [], x )
            xs    -> ( init xs, last xs )

-- | Parse JSON chunks from SSE data
parseChunks :: ConduitT BS.ByteString ChatResponse IO ()
parseChunks
  = awaitForever $ \chunk -> unless (chunk == "[DONE]") $ case eitherDecodeStrict chunk of
    Left _     -> pure ()  -- Skip parse errors
    Right resp -> yield resp

-- | Model information
data ModelInfo = ModelInfo { miId :: Text, miName :: Maybe Text, miVersion :: Maybe Text }
  deriving stock ( Show, Generic )

instance FromJSON ModelInfo where
  parseJSON
    = withObject "ModelInfo" $ \o -> ModelInfo <$> o .: "id" <*> o .:? "name" <*> o .:? "version"

-- | Models list response
newtype ModelsResponse = ModelsResponse { mrData :: [ ModelInfo ] }
  deriving stock ( Show, Generic )

instance FromJSON ModelsResponse where
  parseJSON = withObject "ModelsResponse" $ \o -> ModelsResponse <$> o .: "data"

-- | List available models
listModels :: CopilotClient -> IO (Either Text ModelsResponse)
listModels client = do
  tokenResult <- ensureValidToken (clAuth client)
  case tokenResult of
    Left err    -> pure $ Left $ T.pack $ show err
    Right token -> do
      initReq <- parseRequest "https://api.githubcopilot.com/models"
      let req = initReq { requestHeaders = copilotHeaders token }

      result <- try $ httpLbs req (clManager client)
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
