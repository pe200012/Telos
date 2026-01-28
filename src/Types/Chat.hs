{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Chat
  ( Role(..)
  , Message
  , defaultMessage
  , mkMessage
  , HasRole(..)
  , HasContent(..)
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Aeson      ( FromJSON(parseJSON)
                                 , ToJSON(toJSON)
                                 , Value(String)
                                 , defaultOptions
                                 , fieldLabelModifier
                                 , genericParseJSON
                                 , genericToJSON
                                 , withText
                                 )

import           Relude

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

data Message = Message { _role :: Role, _content :: Text }
  deriving ( Eq, Show, Generic )

defaultMessage :: Message
defaultMessage = Message { _role = User, _content = "" }

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeFieldsNoPrefix ''Message

mkMessage :: Role -> Text -> Message
mkMessage r c = Message { _role = r, _content = c }
