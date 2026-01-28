{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module LLM.Http
  ( runLLMHttp
  , runLLMHttpSilent
  , runLLMHttpWithTools
  , runLLMHttpSilentWithTools
  , ChatRequest
  , defaultChatRequest
  , Message
  , Role(..)
  , supportsToolCalls
  ) where

import           Config                   ( Config
                                          , HasApiKey(..)
                                          , HasBaseUrl(..)
                                          , HasModel(..)
                                          , HasProvider(..)
                                          , HasTemperature(..)
                                          , Provider(..)
                                          )

import           Control.Lens             ( (.~), (^.), (^?), view )
import           Control.Lens.TH          ( makeFieldsNoPrefix )

import           Data.Aeson               ( Options
                                          , Result(..)
                                          , ToJSON(toJSON)
                                          , Value
                                          , defaultOptions
                                          , eitherDecodeStrict'
                                          , fieldLabelModifier
                                          , fromJSON
                                          , genericToJSON
                                          , omitNothingFields
                                          )
import           Data.Aeson.Lens          ( _String, key, nth )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.Conduit             ( (.|), runConduitRes )
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List        as CL
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TIO

import           Effects.LLM              ( LLM(AskLLM)
                                          , LLMResponse
                                          , defaultLLMResponse
                                          , responseText
                                          , responseToolCalls
                                          )

import qualified Network.HTTP.Client      as HTTP
import           Network.HTTP.Simple      ( getResponseBody
                                          , httpSource
                                          , parseRequest
                                          , setRequestBodyJSON
                                          , setRequestHeader
                                          , setRequestMethod
                                          )

import           Polysemy                 ( Embed, Members, Sem, embed, interpret )
import           Polysemy.Input           ( Input, input )
import           Polysemy.State           ( State, get )

import           Relude                   hiding ( State, get, hFlush, stdout )

import           System.IO                ( hFlush, stdout )

import           Tool.Schema              ( ToolSpec )

import           Types.Chat               ( Message, Role(..) )
import           Types.ToolCall           ( ToolCall
                                          , functionArguments
                                          , functionName
                                          , toolCallId
                                          , toolFunction
                                          )

data ChatRequest
  = ChatRequest { _model       :: Text
                , _messages    :: [ Message ]
                , _temperature :: Double
                , _stream      :: Bool
                , _tools       :: Maybe [ ToolSpec ]
                , _toolChoice  :: Maybe Text
                }
  deriving ( Eq, Show, Generic )

chatRequestOptions :: Options
chatRequestOptions = defaultOptions { fieldLabelModifier = drop 1, omitNothingFields = True }

instance ToJSON ChatRequest where
  toJSON = genericToJSON chatRequestOptions

defaultChatRequest :: ChatRequest
defaultChatRequest
  = ChatRequest { _model       = ""
                , _messages    = []
                , _temperature = 0.7
                , _stream      = False
                , _tools       = Nothing
                , _toolChoice  = Nothing
                }

makeFieldsNoPrefix ''ChatRequest

runLLMHttp
  :: Members '[ Embed IO, Input Config, State [ Message ] ] r => Sem (LLM ': r) a -> Sem r a
runLLMHttp = runLLMHttpWithTools Nothing

runLLMHttpSilent
  :: Members '[ Embed IO, Input Config, State [ Message ] ] r => Sem (LLM ': r) a -> Sem r a
runLLMHttpSilent = runLLMHttpSilentWithTools Nothing

runLLMHttpWithTools :: Members '[ Embed IO, Input Config, State [ Message ] ] r
                    => Maybe [ ToolSpec ]
                    -> Sem (LLM ': r) a
                    -> Sem r a
runLLMHttpWithTools toolSpecs = interpret $ \case
  AskLLM msg -> do
    cfg <- input @Config
    history <- get
    let messagesToSend
          = if null history
            then [ msg ]
            else reverse history
        payload
          = defaultChatRequest
          & model .~ view model cfg
          & messages .~ messagesToSend
          & temperature .~ view temperature cfg
          & stream .~ True
          & tools .~ toolSpecs
          & toolChoice .~ toolChoiceValue cfg toolSpecs
    req0 <- embed @IO $ parseRequest (Text.unpack (view baseUrl cfg))
    let req
          = setRequestMethod "POST"
          $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 (view apiKey cfg) ]
          $ setRequestHeader "Content-Type" [ "application/json" ]
          $ setRequestHeader "Accept" [ "text/event-stream" ]
          $ setRequestBodyJSON payload req0
    embed @IO $ streamResponse req

runLLMHttpSilentWithTools :: Members '[ Embed IO, Input Config, State [ Message ] ] r
                          => Maybe [ ToolSpec ]
                          -> Sem (LLM ': r) a
                          -> Sem r a
runLLMHttpSilentWithTools toolSpecs = interpret $ \case
  AskLLM msg -> do
    cfg <- input @Config
    history <- get
    let messagesToSend
          = if null history
            then [ msg ]
            else reverse history
        payload
          = defaultChatRequest
          & model .~ view model cfg
          & messages .~ messagesToSend
          & temperature .~ view temperature cfg
          & stream .~ True
          & tools .~ toolSpecs
          & toolChoice .~ toolChoiceValue cfg toolSpecs
    req0 <- embed @IO $ parseRequest (Text.unpack (view baseUrl cfg))
    let req
          = setRequestMethod "POST"
          $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 (view apiKey cfg) ]
          $ setRequestHeader "Content-Type" [ "application/json" ]
          $ setRequestHeader "Accept" [ "text/event-stream" ]
          $ setRequestBodyJSON payload req0
    embed @IO $ streamResponseSilent req

streamResponse :: HTTP.Request -> IO LLMResponse
streamResponse req = do
  chunks <- runConduitRes
    $ httpSource req getResponseBody
    .| C.linesUnboundedAscii
    .| C.filter (BS8.isPrefixOf "data: ")
    .| C.map (BS8.drop 6)
    .| C.takeWhile (/= "[DONE]")
    .| CL.mapMaybe decodeChunk
    .| C.mapM (\chunk -> liftIO $ do
                 for_ (chunkText chunk) TIO.putStr
                 when (isChunkText chunk) (hFlush stdout)
                 pure chunk)
    .| C.sinkList
  let reply = foldChunks chunks
  if Text.null (reply ^. responseText) && null (reply ^. responseToolCalls)
    then do
      TIO.putStrLn "[no content in response]"
      pure reply
    else do
      unless (Text.null (reply ^. responseText)) (TIO.putStrLn "")
      pure reply

streamResponseSilent :: HTTP.Request -> IO LLMResponse
streamResponseSilent req = do
  chunks <- runConduitRes
    $ httpSource req getResponseBody
    .| C.linesUnboundedAscii
    .| C.filter (BS8.isPrefixOf "data: ")
    .| C.map (BS8.drop 6)
    .| C.takeWhile (/= "[DONE]")
    .| CL.mapMaybe decodeChunk
    .| C.sinkList
  pure (foldChunks chunks)

data Chunk = Chunk { chunkText :: Maybe Text, chunkToolCalls :: [ ToolCall ] }
  deriving ( Eq, Show )

decodeChunk :: BS.ByteString -> Maybe Chunk
decodeChunk bs = do
  value <- eitherToMaybe $ eitherDecodeStrict' @Value bs
  let textPart
        = asum
          [ value ^? key "choices" . nth 0 . key "delta" . key "content" . _String
          , value ^? key "choices" . nth 0 . key "message" . key "content" . _String
          , ("[api error: " <>) <$> value ^? key "error" . key "message" . _String
          ]
      toolCalls = extractToolCalls value
  pure Chunk { chunkText = textPart, chunkToolCalls = toolCalls }

isChunkText :: Chunk -> Bool
isChunkText chunk = isJust (chunkText chunk)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

extractToolCalls :: Value -> [ ToolCall ]
extractToolCalls value = fromMaybe [] $ do
  let deltaCalls   = value ^? key "choices" . nth 0 . key "delta" . key "tool_calls"
      messageCalls = value ^? key "choices" . nth 0 . key "message" . key "tool_calls"
  tcValue <- deltaCalls <|> messageCalls
  case fromJSON tcValue of
    Success calls -> Just calls
    Error _       -> Nothing

foldChunks :: [ Chunk ] -> LLMResponse
foldChunks chunks
  = defaultLLMResponse
  & responseText .~ Text.concat (mapMaybe chunkText chunks)
  & responseToolCalls .~ mergeToolCalls (concatMap chunkToolCalls chunks)

mergeToolCalls :: [ ToolCall ] -> [ ToolCall ]
mergeToolCalls calls = Map.elems $ foldl' insertCall Map.empty calls
  where
    insertCall acc call = Map.insertWith combine (call ^. toolCallId) call acc

    combine newer older
      = let
          mergedArgs
            = (older ^. toolFunction . functionArguments)
            <> (newer ^. toolFunction . functionArguments)
          mergedName
            = if Text.null (older ^. toolFunction . functionName)
              then newer ^. toolFunction . functionName
              else older ^. toolFunction . functionName
          mergedFunc
            = (older ^. toolFunction)
            & functionName .~ mergedName
            & functionArguments .~ mergedArgs
        in 
          older & toolFunction .~ mergedFunc

supportsToolCalls :: Config -> Bool
supportsToolCalls cfg = case cfg ^. provider of
  ZhipuAI -> True
  OpenAI  -> True

toolChoiceValue :: Config -> Maybe [ ToolSpec ] -> Maybe Text
toolChoiceValue cfg toolSpecs
  | supportsToolCalls cfg && maybe False (not . null) toolSpecs = Just "auto"
  | otherwise = Nothing
