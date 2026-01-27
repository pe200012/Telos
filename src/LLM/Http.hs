{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LLM.Http ( runLLMHttp, Request(..), Message(..), Role(..) ) where

import           Control.Applicative      ( asum )
import           Control.Lens             ( (^?) )

import           Data.Aeson               ( FromJSON(parseJSON)
                                          , ToJSON(toJSON)
                                          , Value(String)
                                          , eitherDecode
                                          , withText
                                          )
import           Data.Aeson.Lens          ( _String, key, nth )
import qualified Data.ByteString.Lazy     as LBS
import           Data.Maybe               ( fromMaybe )
import           Data.Text                ( Text )
import qualified Data.Text                as Text
import           Data.Text.Encoding       ( decodeUtf8With, encodeUtf8 )
import           Data.Text.Encoding.Error ( lenientDecode )

import           Effects.LLM              ( LLM(AskLLM) )

import           GHC.Generics             ( Generic )

import           Network.HTTP.Simple      ( getResponseBody
                                          , httpLbs
                                          , parseRequest
                                          , setRequestBodyJSON
                                          , setRequestHeader
                                          , setRequestMethod
                                          )

import           Polysemy                 ( Embed, Member, Sem, embed, interpret )

import           System.Environment       ( lookupEnv )

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

data Request = Request { model :: Text, messages :: [ Message ] }
  deriving ( Eq, Show, Generic )

instance ToJSON Request

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
              = Request
              { model = "glm-4.7-flash", messages = [ Message { role = User, content = prompt } ] }
        let req
              = setRequestMethod "POST"
              $ setRequestHeader "Authorization" [ "Bearer " <> encodeUtf8 apiKey ]
              $ setRequestHeader "Content-Type" [ "application/json" ]
              $ setRequestBodyJSON payload req0
        response <- embed @IO $ httpLbs req
        let body = getResponseBody response
            raw
              = "[no content in response: "
              <> decodeUtf8With lenientDecode (LBS.toStrict body)
              <> "]"
        case eitherDecode body of
          Left err -> pure $ "[error decoding response: " <> Text.pack err <> "]"
          Right (value :: Value) -> pure
            $ fromMaybe raw
            $ asum
              [ value ^? key "choices" . nth 0 . key "message" . key "content" . _String
              , ("[api error: " <>) <$> value ^? key "error" . key "message" . _String
              ]
