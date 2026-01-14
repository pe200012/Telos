{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Provider.Mistral ( newProvider ) where

import           Conduit

import           Control.Exception           ( try )
import           Control.Lens                ( (^.), non )

import qualified Data.Aeson                  as A
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit ( bodyReaderSource )
import           Network.HTTP.Types.Status   ( statusCode )

import           Relude

import           Telos.Config.Types          ( ProviderConfig, pcApiKey, pcBaseURL )
import           Telos.Core.Error            ( LLMError(..) )
import           Telos.Core.Types            ( AssistantMessage, Message, StreamEvent(..), Tool )
import qualified Telos.LLM.Copilot.Client    as CopilotClient
import           Telos.LLM.Provider.Types    ( Provider(..), ProviderType(..) )

-- | Create Mistral provider from config
-- Mistral API is OpenAI-compatible, so we reuse OpenAI implementation
newProvider :: Text -> ProviderConfig -> Manager -> IO (Either LLMError Provider)
newProvider model config manager = do
  case config ^. pcApiKey of
    Nothing     -> pure $ Left $ LLMProviderNotConfigured "Mistral API key not configured"
    Just apiKey -> do
      let baseUrl = config ^. pcBaseURL . non "https://api.mistral.ai/v1"

      pure
        $ Right
        $ Provider
        { providerType = Mistral
        , providerModel = model
        , providerComplete = mistralComplete apiKey baseUrl model manager
        , providerCompleteStreaming = mistralCompleteStreaming apiKey baseUrl model manager
        }

-- | Send non-streaming chat completion request (Mistral-compatible)
mistralComplete :: Text
                -> Text
                -> Text
                -> Manager
                -> [ Message ]
                -> [ Tool ]
                -> IO (Either LLMError AssistantMessage)
mistralComplete apiKey baseUrl model manager messages tools = do
  req <- buildRequest baseUrl apiKey model messages tools False
  result <- try $ httpLbs req manager
  case result of
    Left (_ :: SomeException) -> pure $ Left $ LLMNetworkError "Network error"
    Right resp -> case statusCode (responseStatus resp) of
      200 -> case A.eitherDecode (responseBody resp) of
        Left err -> pure $ Left $ LLMParseError (T.pack err)
        Right
          (chatResp :: CopilotClient.ChatResponse) -> case chatResp ^. CopilotClient.chChoices of
          []           -> pure $ Left $ LLMInvalidResponse "No choices in response"
          (choice : _) -> case choice ^. CopilotClient.chMessage of
            Nothing           -> pure $ Left $ LLMInvalidResponse "No message in choice"
            Just assistantMsg -> pure $ Right assistantMsg
      _   -> pure $ Left $ LLMNetworkError "HTTP error"

-- | Send streaming chat completion request (Mistral-compatible)
mistralCompleteStreaming
  :: Text
  -> Text
  -> Text
  -> Manager
  -> [ Message ]
  -> [ Tool ]
  -> (StreamEvent -> IO ())
  -> IO (ConduitT () StreamEvent IO ())
mistralCompleteStreaming apiKey baseUrl model manager messages tools _callback = do
  req <- buildRequest baseUrl apiKey model messages tools True
  result <- try $ responseOpen req manager
  case result of
    Left (_ :: SomeException) -> pure $ pure ()
    Right resp -> case statusCode (responseStatus resp) of
      200 -> do
        let source = bodyReaderSource (responseBody resp)
        pure $ parseSSESource source .| parseStreamChunks
      _   -> do
        _body <- brConsume (responseBody resp)
        responseClose resp
        pure $ pure ()

-- | Build HTTP request for Mistral API
buildRequest :: Text -> Text -> Text -> [ Message ] -> [ Tool ] -> Bool -> IO Request
buildRequest baseUrl apiKey model messages tools stream = do
  let reqBody
        = A.object
          [ "model" A..= model
          , "messages" A..= messages
          , "tools"
            A..= (if null tools
                    then A.Null
                    else A.toJSON $ map wrapTool tools)
          , "stream" A..= stream
          ]

  parseRequest (T.unpack $ baseUrl <> "/chat/completions") >>= \baseReq -> do
    pure
      $ baseReq { method         = "POST"
                , requestHeaders = [ ( "Content-Type", "application/json" )
                                   , ( "Authorization", "Bearer " <> TE.encodeUtf8 apiKey )
                                   ]
                , requestBody    = RequestBodyLBS (A.encode reqBody)
                }

-- | Parse SSE events from raw bytes
parseSSESource :: ConduitT () BS.ByteString IO () -> ConduitT () BS.ByteString IO ()
parseSSESource source = source .| linesC .| filterC isDataLine .| mapC extractData
  where
    isDataLine bs = "data: " `BS8.isPrefixOf` bs

    extractData   = BS.drop 6

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

-- | Parse stream chunks into StreamEvents
parseStreamChunks :: ConduitT BS.ByteString StreamEvent IO ()
parseStreamChunks = awaitForever $ \chunk -> do
  unless (chunk == "[DONE]") $ case A.eitherDecodeStrict chunk of
    Left _ -> pass
    Right (resp :: CopilotClient.ChatResponse) -> case resp ^. CopilotClient.chChoices of
      []           -> pure ()
      (choice : _) -> case choice ^. CopilotClient.chDelta of
        Nothing    -> pure ()
        Just delta -> do
          case delta ^. CopilotClient.dContent of
            Just content -> yield $ ContentDelta content
            Nothing      -> pure ()
          for_ (delta ^. CopilotClient.dToolCalls) (mapM_ processToolCall)

-- | Process tool call chunk into stream events
processToolCall :: CopilotClient.ToolCallChunk -> ConduitT BS.ByteString StreamEvent IO ()
processToolCall chunk = do
  case ( chunk ^. CopilotClient.tccId, chunk ^. CopilotClient.tccFunction ) of
    ( Just tcId, Just fn ) -> case fn ^. CopilotClient.fcName of
      Just name -> yield $ ToolCallStart (chunk ^. CopilotClient.tccIndex) tcId name
      Nothing   -> pure ()
    _ -> pure ()
  case chunk ^. CopilotClient.tccFunction of
    Just fn -> case fn ^. CopilotClient.fcArguments of
      Just args -> yield $ ToolCallDelta (chunk ^. CopilotClient.tccIndex) args
      Nothing   -> pure ()
    Nothing -> pure ()

-- | Wrap tool in OpenAI-compatible format (for Mistral)
wrapTool :: Tool -> A.Value
wrapTool tool = A.object [ "type" A..= ("function" :: Text), "function" A..= tool ]
