{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.ToolCall
  ( ToolCall
  , ToolFunction
  , defaultToolCall
  , HasToolCallId(..)
  , HasToolCallType(..)
  , HasToolFunction(..)
  , HasFunctionName(..)
  , HasFunctionArguments(..)
  , mkToolCall
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Aeson      ( (.:)
                                 , (.=)
                                 , FromJSON(parseJSON)
                                 , ToJSON(toJSON)
                                 , object
                                 , withObject
                                 )

import           Relude

data ToolFunction = ToolFunction { _functionName :: Text, _functionArguments :: Text }
  deriving ( Eq, Show, Generic )

data ToolCall
  = ToolCall { _toolCallId :: Text, _toolCallType :: Text, _toolFunction :: ToolFunction }
  deriving ( Eq, Show, Generic )

instance ToJSON ToolFunction where
  toJSON fn = object [ "name" .= _functionName fn, "arguments" .= _functionArguments fn ]

instance FromJSON ToolFunction where
  parseJSON = withObject "ToolFunction" $ \v -> ToolFunction <$> v .: "name" <*> v .: "arguments"

instance ToJSON ToolCall where
  toJSON call
    = object
      [ "id" .= _toolCallId call, "type" .= _toolCallType call, "function" .= _toolFunction call ]

instance FromJSON ToolCall where
  parseJSON
    = withObject "ToolCall" $ \v -> ToolCall <$> v .: "id" <*> v .: "type" <*> v .: "function"

makeFieldsNoPrefix ''ToolFunction

makeFieldsNoPrefix ''ToolCall

defaultToolCall :: ToolCall
defaultToolCall
  = ToolCall { _toolCallId = "", _toolCallType = "function", _toolFunction = ToolFunction "" "" }

mkToolCall :: Text -> Text -> Text -> ToolCall
mkToolCall callId name args
  = ToolCall
  { _toolCallId = callId, _toolCallType = "function", _toolFunction = ToolFunction name args }
