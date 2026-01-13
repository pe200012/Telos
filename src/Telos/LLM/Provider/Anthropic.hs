{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Provider.Anthropic (newProvider) where

import           Conduit
import qualified Data.Aeson                as A
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

import           Control.Exception         ( try )
import           Control.Lens              ( (^.), non, view )
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit ( bodyReaderSource )
import           Network.HTTP.Types.Status ( statusCode )
import           Relude

import           Telos.Config.Types        ( ProviderConfig, pcApiKey, pcBaseURL )
import           Telos.Core.Error          ( LLMError(..) )
import           Telos.Core.Types          ( Message(..), Tool, AssistantMessage, StreamEvent(..)
                                            , makeAssistantMessage
                                            , toolName, toolDescription, toolInputSchema, amContent )
import           Telos.LLM.Provider.Types  ( Provider(..), ProviderType(..) )

-- | Create Anthropic provider from config
newProvider :: Text -> ProviderConfig -> Manager -> IO (Either LLMError Provider)
newProvider model config manager = do
  case config ^. pcApiKey of
    Nothing -> pure $ Left $ LLMProviderNotConfigured "Anthropic API key not configured"
    Just apiKey -> do
      let baseUrl = case config ^. pcBaseURL of
            Just url -> url
            Nothing  -> "https://api.anthropic.com/v1"
      
      pure $ Right $ Provider
        { providerType = Anthropic
        , providerModel = model
        , providerComplete = anthropicComplete apiKey baseUrl model manager
        , providerCompleteStreaming = anthropicCompleteStreaming apiKey baseUrl model manager
        }

-- | Convert Telos messages to Anthropic format
-- Returns (Maybe system prompt, messages array)
convertToAnthropicMessages :: [Message] -> (Maybe Text, [A.Value])
convertToAnthropicMessages msgs = 
  let systemPrompt = listToMaybe [c | SystemMessage c <- msgs]
      messages = flip mapMaybe msgs $ \case
        UserMessage c -> Just $ A.object 
          [ "role" A..= ("user" :: Text)
          , "content" A..= c
          ]
        AssistantMsg am -> Just $ A.object
          [ "role" A..= ("assistant" :: Text)
          , "content" A..= (view amContent am ^. non "")
          ]
        SystemMessage _ -> Nothing  -- Extracted separately
        ToolResultMessage _ _ result _ -> Just $ A.object
          [ "role" A..= ("user" :: Text)
          , "content" A..= result
          ]
  in (systemPrompt, messages)

-- | Convert Telos tools to Anthropic format
convertToAnthropicTools :: [Tool] -> [A.Value]
convertToAnthropicTools = map $ \tool -> A.object
  [ "name" A..= view toolName tool
  , "description" A..= (view toolDescription tool ^. non "")
  , "input_schema" A..= view toolInputSchema tool
  ]

-- | Build HTTP request for Anthropic API
buildRequest :: Text -> Text -> Text -> [Message] -> [Tool] -> Bool -> IO Request
buildRequest baseUrl apiKey model messages tools streaming = do
  let (systemPrompt, anthropicMsgs) = convertToAnthropicMessages messages
      url = T.unpack baseUrl <> "/messages"
      
      body = A.object $ catMaybes
        [ Just $ "model" A..= model
        , Just $ "max_tokens" A..= (1024 :: Int)
        , ("system" A..=) <$> systemPrompt
        , Just $ "messages" A..= anthropicMsgs
        , if null tools then Nothing else Just $ "tools" A..= convertToAnthropicTools tools
        , Just $ "stream" A..= streaming
        ]
  
  initReq <- parseRequest url
  pure $ initReq
    { method = "POST"
    , requestHeaders = 
        [ ("x-api-key", TE.encodeUtf8 apiKey)
        , ("anthropic-version", "2023-06-01")
        , ("content-type", "application/json")
        ]
    , requestBody = RequestBodyLBS $ A.encode body
    }

-- | Parse Anthropic response to AssistantMessage
parseAnthropicResponse :: A.Value -> Either Text AssistantMessage
parseAnthropicResponse val = do
  obj <- case val of
    A.Object o -> Right o
    _ -> Left "Response is not an object"
  
  content <- case KM.lookup "content" obj of
    Just (A.Array arr) -> Right arr
    _ -> Left "No content array in response"
  
  -- Extract text from content blocks
  let textBlocks = flip mapMaybe (toList content) $ \block -> case block of
        A.Object o -> case KM.lookup "type" o of
          Just (A.String "text") -> KM.lookup "text" o >>= \case
            A.String t -> Just t
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
  
  let fullText = T.intercalate "\n" textBlocks
  pure $ makeAssistantMessage (if T.null fullText then Nothing else Just fullText) []

-- | Send non-streaming chat completion request
anthropicComplete
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> IO (Either LLMError AssistantMessage)
anthropicComplete apiKey baseUrl model manager messages tools = do
  req <- buildRequest baseUrl apiKey model messages tools False
  result <- try $ httpLbs req manager
  case result of
    Left (_ :: SomeException) -> pure $ Left $ LLMNetworkError "Network error"
    Right resp -> case statusCode (responseStatus resp) of
      200 -> case A.eitherDecode (responseBody resp) of
        Left err -> pure $ Left $ LLMParseError (T.pack err)
        Right val -> case parseAnthropicResponse val of
          Left err -> pure $ Left $ LLMInvalidResponse err
          Right assistantMsg -> pure $ Right assistantMsg
      _ -> pure $ Left $ LLMNetworkError "HTTP error"

-- | Parse SSE stream from Anthropic
parseSSESource :: ConduitT BS.ByteString A.Value IO ()
parseSSESource = do
  linesUnboundedAsciiC .| concatMapC parseLine
  where
    parseLine line
      | "data: " `BS8.isPrefixOf` line =
          let jsonPart = BS.drop 6 line
          in maybeToList (A.decodeStrict jsonPart)
      | otherwise = []

-- | Parse Anthropic stream chunks to StreamEvents
parseStreamChunks :: ConduitT A.Value StreamEvent IO ()
parseStreamChunks = awaitForever $ \val -> case val of
  A.Object obj -> do
    case KM.lookup "type" obj of
      Just (A.String "content_block_delta") -> do
        case KM.lookup "delta" obj of
          Just (A.Object delta) -> case KM.lookup "type" delta of
            Just (A.String "text_delta") -> case KM.lookup "text" delta of
              Just (A.String txt) -> yield $ ContentDelta txt
              _ -> pure ()
            _ -> pure ()
          _ -> pure ()
      Just (A.String "message_stop") -> pure ()
      Just (A.String "ping") -> yield Ping
      _ -> pure ()
  _ -> pure ()

-- | Send streaming chat completion request
anthropicCompleteStreaming
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> (StreamEvent -> IO ())
  -> IO (ConduitT () StreamEvent IO ())
anthropicCompleteStreaming apiKey baseUrl model manager messages tools callback = do
  req <- buildRequest baseUrl apiKey model messages tools True
  result <- try $ responseOpen req manager
  case result of
    Left (_ :: SomeException) -> pure $ pure ()
    Right resp -> case statusCode (responseStatus resp) of
      200 -> do
        let source = bodyReaderSource (responseBody resp)
              .| parseSSESource
              .| parseStreamChunks
              .| mapMC (\event -> callback event >> pure event)
        pure source
      _ -> pure $ pure ()
