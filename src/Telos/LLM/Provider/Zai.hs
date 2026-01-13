{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Provider.Zai (newProvider) where

import           Conduit
import qualified Data.Aeson                as A
import qualified Data.Aeson.KeyMap         as KM
import           Data.Aeson.Lens
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

import           Control.Exception         ( try )
import           Control.Lens              ( (^.), non, view, (^?), ix, at, _Just,  )
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit ( bodyReaderSource )
import           Network.HTTP.Types.Status ( statusCode )
import           Relude

import           Telos.Config.Types        ( ProviderConfig, pcApiKey, pcBaseURL )
import           Telos.Core.Error          ( LLMError(..) )
import           Telos.Core.Types          ( Message(..), Tool, AssistantMessage, StreamEvent(..)
                                            , makeAssistantMessage
                                            , toolName, toolDescription, toolInputSchema )
import           Telos.LLM.Provider.Types  ( Provider(..), ProviderType(..) )
import Data.Char (isSpace)
import Control.Error (note)

-- | Create Z.AI provider from config
newProvider :: Text -> ProviderConfig -> Manager -> IO (Either LLMError Provider)
newProvider model config manager = do
  case config ^. pcApiKey of
    Nothing -> pure $ Left $ LLMProviderNotConfigured "Z.AI API key not configured"
    Just apiKey -> do
      let baseUrl = case config ^. pcBaseURL of
            Just url -> url
            Nothing  -> "https://api.z.ai/v1"

      pure $ Right $ Provider
        { providerType = Zai
        , providerModel = model
        , providerComplete = zaiComplete apiKey baseUrl model manager
        , providerCompleteStreaming = zaiCompleteStreaming apiKey baseUrl model manager
        }

-- | Build HTTP request for Z.AI API
-- Z.AI uses OpenAI-compatible format
buildRequest :: Text -> Text -> Text -> [Message] -> [Tool] -> Bool -> IO Request
buildRequest baseUrl apiKey model messages tools streaming = do
  let url = T.unpack baseUrl <> "/chat/completions"
      toolsField = if null tools then [] else ["tools" A..= (convertTools <$> tools)]
      body = A.object $
        [ "model" A..= model
        , "messages" A..= messages  -- Telos Message already compatible
        , "stream" A..= streaming
        ] <> toolsField

  initReq <- parseRequest url
  pure $ initReq
    { method = "POST"
    , requestHeaders =
        [ ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
        , ("content-type", "application/json")
        ]
    , requestBody = RequestBodyLBS $ A.encode body
    }

-- | Convert Tool to Z.AI format (same as OpenAI)
convertTools :: Tool -> A.Value
convertTools tool = A.object
  [ "type" A..= ("function" :: Text)
  , "function" A..= A.object
      [ "name" A..= view toolName tool
      , "description" A..= (view toolDescription tool ^. non "")
      , "parameters" A..= view toolInputSchema tool
      ]
  ]

-- | Parse Z.AI response to AssistantMessage
-- Z.AI response format is OpenAI-compatible
parseZaiResponse :: A.Value -> Either Text AssistantMessage
parseZaiResponse val@A.Object{} = do
  choice <- val ^? key "choices" . _Array & note "No choices array in response"

  firstChoice <- choice ^? ix 0 . _Object & note "Empty choices array"

  message <- firstChoice ^? at "message" . _Just . _Object & note "No message in choice"

  -- Extract content and tool_calls from message
  contentM <- case KM.lookup "content" message of
    Just (A.String txt) -> Right $ Just txt
    Just A.Null -> Right Nothing
    _ -> Left "Invalid content in message"

  -- Parse tool_calls if present (currently ignored)
  _toolCallsM <- case KM.lookup "tool_calls" message of
    Just (A.Array arr) -> Right $ Just arr
    Just A.Null -> Right Nothing
    _ -> Right Nothing

  pure $ makeAssistantMessage contentM []
parseZaiResponse _ = Left "Response is not an object"

-- | Send non-streaming chat completion request
zaiComplete
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> IO (Either LLMError AssistantMessage)
zaiComplete apiKey baseUrl model manager messages tools = do
  req <- buildRequest baseUrl apiKey model messages tools False
  result <- try $ httpLbs req manager
  case result of
    Left (_ :: SomeException) -> pure $ Left $ LLMNetworkError "Network error"
    Right resp -> case statusCode (responseStatus resp) of
      200 -> case A.eitherDecode (responseBody resp) of
        Left err -> pure $ Left $ LLMParseError (T.pack err)
        Right val -> case parseZaiResponse val of
          Left err -> pure $ Left $ LLMInvalidResponse err
          Right assistantMsg -> pure $ Right assistantMsg
      _ -> pure $ Left $ LLMNetworkError "HTTP error"

-- | Parse SSE stream from Z.AI
-- Z.AI uses OpenAI-compatible SSE format with [DONE] marker
parseSSESource :: ConduitT BS.ByteString A.Value IO ()
parseSSESource = do
  linesUnboundedAsciiC .| concatMapC parseLine
  where
    parseLine line
      | "data: " `BS8.isPrefixOf` line =
          let jsonPart = BS.drop 6 line
              trimmed = BS8.dropWhile isSpace jsonPart
          in if trimmed == "[DONE]"
              then []
              else maybeToList (A.decodeStrict trimmed)
      | otherwise = []

-- | Parse Z.AI stream chunks to StreamEvents
-- OpenAI-compatible streaming format
parseStreamChunks :: ConduitT A.Value StreamEvent IO ()
parseStreamChunks = awaitForever $ \case
  A.Object obj ->
    -- Check for choices
    case KM.lookup "choices" obj of
      Just (A.Array choices) -> forM_ (toList choices) processChoice

      _ -> pure ()
  _ -> pure ()
  where
    processChoice choice =  maybe (pure ()) processDelta (choice ^? key "delta" . _Object)

    processDelta delta = do
      -- Content delta
      case KM.lookup "content" delta of
        Just (A.String txt) -> yield $ ContentDelta txt
        _ -> pure ()

      -- Tool call delta (if supported)
      case KM.lookup "tool_calls" delta of
        Just (A.Array arr) -> forM_ (toList arr) processToolCall
        _ -> pure ()

    processToolCall tc = case tc of
      A.Object tcObj ->
        case KM.lookup "type" tcObj of
          Just (A.String "function") ->
            case extractToolCallInfo tc of
              Just (idx, args) ->
                yield $ ToolCallDelta idx args
              _ -> pure ()
          _ -> pure ()
      _ -> pure ()

    extractToolCallInfo tcObj = do
      idxNum <- tcObj ^? key "index" . _Number
      let idx = round idxNum
      funcObj <- tcObj ^? key "function" . _Value
      args <- funcObj ^? key "arguments" . _String
      pure (idx, args)

-- | Send streaming chat completion request
zaiCompleteStreaming
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> (StreamEvent -> IO ())
  -> IO (ConduitT () StreamEvent IO ())
zaiCompleteStreaming apiKey baseUrl model manager messages tools callback = do
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
