{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Chat
  ( Role(..)
  , Message
  , defaultMessage
  , mkMessage
  , mkToolCallMessage
  , mkToolResultMessage
  , HasRole(..)
  , HasContent(..)
  , HasToolCallId(..)
  , HasToolCalls(..)
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Aeson      ( FromJSON(parseJSON)
                                 , Options
                                 , ToJSON(toJSON)
                                 , Value(String)
                                 , camelTo2
                                 , defaultOptions
                                 , fieldLabelModifier
                                 , genericParseJSON
                                 , genericToJSON
                                 , withText
                                 )

import           Relude

import           Types.ToolCall  ( ToolCall )

data Role = System | User | Assistant | Tool
  deriving ( Eq, Show )

instance ToJSON Role where
  toJSON System    = String "system"
  toJSON User      = String "user"
  toJSON Assistant = String "assistant"
  toJSON Tool      = String "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "system"    -> pure System
    "user"      -> pure User
    "assistant" -> pure Assistant
    "tool"      -> pure Tool
    _           -> fail "unknown role"

data Message
  = Message
  { _role :: Role, _content :: Text, _toolCallId :: Maybe Text, _toolCalls :: Maybe [ ToolCall ] }
  deriving ( Eq, Show, Generic )

defaultMessage :: Message
defaultMessage
  = Message { _role = User, _content = "", _toolCallId = Nothing, _toolCalls = Nothing }

messageOptions :: Options
messageOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 1 }

instance ToJSON Message where
  toJSON = genericToJSON messageOptions

instance FromJSON Message where
  parseJSON = genericParseJSON messageOptions

makeFieldsNoPrefix ''Message

mkMessage :: Role -> Text -> Message
mkMessage r c = Message { _role = r, _content = c, _toolCallId = Nothing, _toolCalls = Nothing }

mkToolCallMessage :: [ ToolCall ] -> Message
mkToolCallMessage calls
  = Message { _role = Assistant, _content = "", _toolCallId = Nothing, _toolCalls = Just calls }

mkToolResultMessage :: Text -> Text -> Message
mkToolResultMessage callId result
  = Message { _role = Tool, _content = result, _toolCallId = Just callId, _toolCalls = Nothing }
