{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LLM.Http ( runLLMHttp, ChatRequest(..), Message(..), Role(..) ) where

import           Control.Applicative      ( asum )
import           Control.Lens             ( (^?) )
import           Control.Monad.IO.Class   ( liftIO )

import           Data.Aeson               ( FromJSON(parseJSON)
                                          , ToJSON(toJSON)
                                          , Value(String)
                                          , eitherDecodeStrict'
                                          , withText
                                          )
import           Data.Aeson.Lens          ( _String, key, nth )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.Conduit             ( (.|), runConduitRes )
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List        as CL
import           Data.IORef               ( IORef, modifyIORef', newIORef, readIORef, writeIORef )
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

import           Polysemy                 ( Embed, Member, Sem, embed, interpret )

import           System.Environment       ( lookupEnv )
import           System.IO                ( hFlush, stdout )

data Role = System | User | Assistant
  deriving ( Eq, Show )

instance ToJSON Role where
  toJSON System    = String "system"
  toJSON User      = String "user"
  toJSON Assistant = String "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "system"    -> pure System
    "user"      -> pure User
    "assistant" -> pure Assistant
    _           -> fail "unknown role"

data Message = Message { role :: Role, content :: Text }
  deriving ( Eq, Show, Generic )

instance ToJSON Message

instance FromJSON Message

data ChatRequest = ChatRequest { model :: Text, messages :: [ Message ], stream :: Bool }
  deriving ( Eq, Show, Generic )

instance ToJSON ChatRequest

runLLMHttp :: Member (Embed IO) r => Sem (LLM ': r) a -> Sem r a
runLLMHttp = interpret $ \case
  AskLLM prompt -> do
    mKey <- embed @IO $ lookupEnv "ZHIPUAI_API_KEY"
    case mKey of
      Nothing     -> pure "[missing env var: ZHIPUAI_API_KEY]"
      Just rawKey -> do
        let apiKey = Text.pack rawKey
        req0 <- embed @IO $ parseRequest "https://open.bigmodel.cn/api/paas/v4/chat/completions"
        let payload
              = ChatRequest { model    = "glm-4.7-flash"
                            , messages = [ Message { role = User, content = prompt } ]
                            , stream   = True
                            }
        let req
              = setRequestMethod "POST"
              $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 apiKey ]
              $ setRequestHeader "Content-Type" [ "application/json" ]
              $ setRequestBodyJSON payload req0
        embed @IO $ streamResponse req

streamResponse :: HTTP.Request -> IO Text
streamResponse req = do
  fine <- newIORef False
  runConduitRes
    $ httpSource req getResponseBody
    .| C.linesUnboundedAscii
    .| C.filter (BS8.isPrefixOf "data: ")
    .| C.map (BS8.drop 6)
    .| C.takeWhile (/= "[DONE]")
    .| CL.mapMaybe decodeChunk
    .| C.mapM_ (\chunk -> liftIO (TIO.putStr chunk >> writeIORef fine True >> hFlush stdout))
  isFine <- readIORef fine
  if isFine
    then pure Text.empty
    else do
      TIO.putStrLn "[no content in response]"
      pure Text.empty

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
