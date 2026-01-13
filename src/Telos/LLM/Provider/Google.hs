{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Provider.Google (newProvider) where

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

-- | Create Google Gemini provider from config
newProvider :: Text -> ProviderConfig -> Manager -> IO (Either LLMError Provider)
newProvider model config manager = do
  case config ^. pcApiKey of
    Nothing -> pure $ Left $ LLMProviderNotConfigured "Google API key not configured"
    Just apiKey -> do
      let baseUrl = case config ^. pcBaseURL of
            Just url -> url
            Nothing  -> "https://generativelanguage.googleapis.com/v1beta"

      pure $ Right $ Provider
        { providerType = Google
        , providerModel = model
        , providerComplete = googleComplete apiKey baseUrl model manager
        , providerCompleteStreaming = googleCompleteStreaming apiKey baseUrl model manager
        }

-- | Convert Telos messages to Google Gemini format
-- Returns (Maybe system instruction, contents array)
convertToGeminiMessages :: [Message] -> (Maybe Text, [A.Value])
convertToGeminiMessages msgs =
  let systemPrompt = listToMaybe [c | SystemMessage c <- msgs]
      contents = flip mapMaybe msgs $ \case
        UserMessage c -> Just $ A.object
          [ "role" A..= ("user" :: Text)
          , "parts" A..= [A.object ["text" A..= c]]
          ]
        AssistantMsg am -> Just $ A.object
          [ "role" A..= ("model" :: Text)
          , "parts" A..= [A.object ["text" A..= (view amContent am ^. non "")]]
          ]
        SystemMessage _ -> Nothing  -- Extracted separately
        ToolResultMessage _ name result _ -> Just $ A.object
          [ "role" A..= ("user" :: Text)
          , "parts" A..= [A.object
              [ "functionResponse" A..= A.object
                  [ "name" A..= name
                  , "response" A..= A.object ["result" A..= result]
                  ]
              ]]
          ]
  in (systemPrompt, contents)

-- | Convert Telos tools to Google Gemini format
convertToGeminiTools :: [Tool] -> A.Value
convertToGeminiTools tools = A.object
  [ "function_declarations" A..= map convertTool tools
  ]
  where
    convertTool tool = A.object
      [ "name" A..= view toolName tool
      , "description" A..= (view toolDescription tool ^. non "")
      , "parameters" A..= view toolInputSchema tool
      ]

-- | Build HTTP request for Google Gemini API
buildRequest :: Text -> Text -> Text -> [Message] -> [Tool] -> Bool -> IO Request
buildRequest baseUrl apiKey model messages tools streaming = do
  let (systemPrompt, geminiContents) = convertToGeminiMessages messages
      endpoint = if streaming then ":streamGenerateContent" else ":generateContent"
      url = T.unpack baseUrl <> "/models/" <> T.unpack model <> T.unpack endpoint <> "?alt=sse"

      body = A.object $ catMaybes
        [ ("system_instruction" A..=) . (\txt -> A.object ["parts" A..= [A.object ["text" A..= txt]]]) <$> systemPrompt
        , Just $ "contents" A..= geminiContents
        , if null tools then Nothing else Just $ "tools" A..= [convertToGeminiTools tools]
        ]

  initReq <- parseRequest url
  pure $ initReq
    { method = "POST"
    , requestHeaders =
        [ ("x-goog-api-key", TE.encodeUtf8 apiKey)
        , ("content-type", "application/json")
        ]
    , requestBody = RequestBodyLBS $ A.encode body
    }

-- | Parse Google Gemini response to AssistantMessage
parseGeminiResponse :: A.Value -> Either Text AssistantMessage
parseGeminiResponse val = do
  obj <- case val of
    A.Object o -> Right o
    _ -> Left "Response is not an object"

  candidates <- case KM.lookup "candidates" obj of
    Just (A.Array arr) -> Right arr
    _ -> Left "No candidates array in response"

  firstCandidate <- case toList candidates of
    [] -> Left "Empty candidates array"
    (c:_) -> case c of
      A.Object o -> Right o
      _ -> Left "Candidate is not an object"

  content <- case KM.lookup "content" firstCandidate of
    Just (A.Object o) -> Right o
    _ -> Left "No content in candidate"

  parts <- case KM.lookup "parts" content of
    Just (A.Array arr) -> Right arr
    _ -> Left "No parts in content"

  -- Extract text from parts
  let textBlocks = flip mapMaybe (toList parts) $ \part -> case part of
        A.Object o -> KM.lookup "text" o >>= \case
          A.String t -> Just t
          _ -> Nothing
        _ -> Nothing

  let fullText = T.intercalate "\n" textBlocks
  pure $ makeAssistantMessage (if T.null fullText then Nothing else Just fullText) []

-- | Send non-streaming chat completion request
googleComplete
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> IO (Either LLMError AssistantMessage)
googleComplete apiKey baseUrl model manager messages tools = do
  req <- buildRequest baseUrl apiKey model messages tools False
  result <- try $ httpLbs req manager
  case result of
    Left (_ :: SomeException) -> pure $ Left $ LLMNetworkError "Network error"
    Right resp -> case statusCode (responseStatus resp) of
      200 -> case A.eitherDecode (responseBody resp) of
        Left err -> pure $ Left $ LLMParseError (T.pack err)
        Right val -> case parseGeminiResponse val of
          Left err -> pure $ Left $ LLMInvalidResponse err
          Right assistantMsg -> pure $ Right assistantMsg
      _ -> pure $ Left $ LLMNetworkError "HTTP error"

-- | Parse SSE stream from Google Gemini
parseSSESource :: ConduitT BS.ByteString A.Value IO ()
parseSSESource = do
  linesUnboundedAsciiC .| concatMapC parseLine
  where
    parseLine line
      | "data: " `BS8.isPrefixOf` line =
          let jsonPart = BS.drop 6 line
          in maybeToList (A.decodeStrict jsonPart)
      | otherwise = []

-- | Parse Gemini stream chunks to StreamEvents
parseStreamChunks :: ConduitT A.Value StreamEvent IO ()
parseStreamChunks = awaitForever $ \case
  A.Object obj -> do
    case KM.lookup "candidates" obj of
      Just (A.Array candidates) -> forM_ (toList candidates) $ \case
        A.Object candObj -> case KM.lookup "content" candObj of
          Just (A.Object content) -> case KM.lookup "parts" content of
            Just (A.Array parts) -> forM_ (toList parts) $ \case
              A.Object partObj -> case KM.lookup "text" partObj of
                Just (A.String txt) -> yield $ ContentDelta txt
                _ -> pure ()
              _ -> pure ()
            _ -> pure ()
          _ -> pure ()
        _ -> pure ()
      _ -> pure ()
  _ -> pure ()

-- | Send streaming chat completion request
googleCompleteStreaming
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [Message]
  -> [Tool]
  -> (StreamEvent -> IO ())
  -> IO (ConduitT () StreamEvent IO ())
googleCompleteStreaming apiKey baseUrl model manager messages tools callback = do
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
