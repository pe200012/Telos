{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.LLM
  ( LLM(..)
  , LLMResponse
  , defaultLLMResponse
  , HasResponseText(..)
  , HasResponseToolCalls(..)
  , askLLM
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Polysemy        ( makeSem )

import           Relude

import           Types.Chat      ( Message )
import           Types.ToolCall  ( ToolCall )

data LLMResponse = LLMResponse { _responseText :: Text, _responseToolCalls :: [ ToolCall ] }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''LLMResponse

defaultLLMResponse :: LLMResponse
defaultLLMResponse = LLMResponse { _responseText = "", _responseToolCalls = [] }

data LLM m a where
  -- | Send a message and receive a response payload.
  AskLLM :: Message -> LLM m LLMResponse

makeSem ''LLM
