{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module LLM.Http ( runLLMHttp, runLLMHttpSilent, ChatRequest(..), Message(..), Role(..) ) where

import           Config                   ( Config
                                          , HasApiKey(..)
                                          , HasBaseUrl(..)
                                          , HasModel(..)
                                          , HasTemperature(..)
                                          )

import           Control.Applicative      ( asum )
import           Control.Lens             ( (^?), view )
import           Control.Lens.TH          ( makeFieldsNoPrefix )
import           Control.Monad.IO.Class   ( liftIO )

import           Data.Aeson               ( ToJSON(toJSON)
                                          , Value
                                          , defaultOptions
                                          , eitherDecodeStrict'
                                          , fieldLabelModifier
                                          , genericToJSON
                                          )
import           Data.Aeson.Lens          ( _String, key, nth )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.Conduit             ( (.|), runConduitRes )
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List        as CL
import           Data.IORef               ( modifyIORef', newIORef, readIORef, writeIORef )
import           Data.Text                ( Text )
import qualified Data.Text                as Text
import           Data.Text.Encoding       ( encodeUtf8 )
import qualified Data.Text.IO             as TIO

import           Effects.LLM              ( LLM(AskLLM) )

import           GHC.Generics             ( Generic )

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

import           System.IO                ( hFlush, stdout )

import           Types.Chat               ( Message(..), Role(..) )

data ChatRequest
  = ChatRequest
  { _model :: Text, _messages :: [ Message ], _temperature :: Double, _stream :: Bool }
  deriving ( Eq, Show, Generic )

instance ToJSON ChatRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

makeFieldsNoPrefix ''ChatRequest

runLLMHttp
  :: Members '[ Embed IO, Input Config, State [ Message ] ] r => Sem (LLM ': r) a -> Sem r a
runLLMHttp = interpret $ \case
  AskLLM msg -> do
    cfg <- input @Config
    history <- get
    let messagesToSend
          = if null history
            then [ msg ]
            else reverse history
    req0 <- embed @IO $ parseRequest (Text.unpack (view baseUrl cfg))
    let payload
          = ChatRequest { _model       = view model cfg
                        , _messages    = messagesToSend
                        , _temperature = view temperature cfg
                        , _stream      = True
                        }
    let req
          = setRequestMethod "POST"
          $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 (view apiKey cfg) ]
          $ setRequestHeader "Content-Type" [ "application/json" ]
          $ setRequestHeader "Accept" [ "text/event-stream" ]
          $ setRequestBodyJSON payload req0
    embed @IO $ streamResponse req

runLLMHttpSilent
  :: Members '[ Embed IO, Input Config, State [ Message ] ] r => Sem (LLM ': r) a -> Sem r a
runLLMHttpSilent = interpret $ \case
  AskLLM msg -> do
    cfg <- input @Config
    history <- get
    let messagesToSend
          = if null history
            then [ msg ]
            else reverse history
    req0 <- embed @IO $ parseRequest (Text.unpack (view baseUrl cfg))
    let payload
          = ChatRequest { _model       = view model cfg
                        , _messages    = messagesToSend
                        , _temperature = view temperature cfg
                        , _stream      = True
                        }
    let req
          = setRequestMethod "POST"
          $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 (view apiKey cfg) ]
          $ setRequestHeader "Content-Type" [ "application/json" ]
          $ setRequestHeader "Accept" [ "text/event-stream" ]
          $ setRequestBodyJSON payload req0
    embed @IO $ streamResponseSilent req

streamResponse :: HTTP.Request -> IO Text
streamResponse req = do
  fineRef <- newIORef False
  chunksRef <- newIORef []
  runConduitRes
    $ httpSource req getResponseBody
    .| C.linesUnboundedAscii
    .| C.filter (BS8.isPrefixOf "data: ")
    .| C.map (BS8.drop 6)
    .| C.takeWhile (/= "[DONE]")
    .| CL.mapMaybe decodeChunk
    .| C.mapM_ (\chunk -> liftIO $ do
                  TIO.putStr chunk
                  modifyIORef' chunksRef (chunk :)
                  writeIORef fineRef True
                  hFlush stdout)
  isFine <- readIORef fineRef
  if isFine
    then do
      chunks <- readIORef chunksRef
      pure (Text.concat (reverse chunks))
    else do
      TIO.putStrLn "[no content in response]"
      pure Text.empty

streamResponseSilent :: HTTP.Request -> IO Text
streamResponseSilent req = do
  chunksRef <- newIORef []
  runConduitRes
    $ httpSource req getResponseBody
    .| C.linesUnboundedAscii
    .| C.filter (BS8.isPrefixOf "data: ")
    .| C.map (BS8.drop 6)
    .| C.takeWhile (/= "[DONE]")
    .| CL.mapMaybe decodeChunk
    .| C.mapM_ (\chunk -> liftIO (modifyIORef' chunksRef (chunk :)))
  chunks <- readIORef chunksRef
  if null chunks
    then pure Text.empty
    else pure (Text.concat (reverse chunks))

decodeChunk :: BS.ByteString -> Maybe Text
decodeChunk bs = do
  value <- eitherToMaybe $ eitherDecodeStrict' @Value bs
  asum
    [ value ^? key "choices" . nth 0 . key "delta" . key "content" . _String
    , value ^? key "choices" . nth 0 . key "message" . key "content" . _String
    , ("[api error: " <>) <$> value ^? key "error" . key "message" . _String
    ]

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
