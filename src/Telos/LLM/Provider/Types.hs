{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Telos.LLM.Provider.Types
  ( ProviderType(..)
  , parseProvider
  , parseModelString
  , Provider(..)
  , providerName
  , complete
  , completeStreaming
  ) where

import           Conduit
import           Data.Aeson                ( ToJSON, FromJSON )
import qualified Data.Text                as T

import           Relude

import           Telos.Core.Error         ( LLMError )
import           Telos.Core.Types         ( Message, Tool, StreamEvent, AssistantMessage )

-- | Supported LLM providers
data ProviderType
  = OpenAI
  | Anthropic
  | Google
  | Mistral
  | Copilot
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( ToJSON, FromJSON )

-- | Parse provider from string
parseProvider :: Text -> Maybe ProviderType
parseProvider "openai"    = Just OpenAI
parseProvider "anthropic" = Just Anthropic
parseProvider "google"    = Just Google
parseProvider "mistral"   = Just Mistral
parseProvider "copilot"   = Just Copilot
parseProvider _           = Nothing

-- | Parse model string into (provider, model)
-- Supports formats: "provider/model" or just "model" (defaults to copilot)
parseModelString :: Text -> (ProviderType, Text)
parseModelString str = case T.splitOn "/" str of
  [providerStr, model] -> case parseProvider providerStr of
    Just provider -> (provider, model)
    Nothing       -> (Copilot, str)  -- Unknown provider → treat as full model string with default provider
  [model] -> (Copilot, model)  -- No provider prefix → default to copilot
  _       -> (Copilot, "gpt-4o")  -- Invalid format → fallback

-- | Unified provider interface
-- Uses record-of-functions pattern for dynamic dispatch
data Provider = Provider
  { providerType      :: ProviderType
  , providerModel     :: Text
  , providerComplete  :: [Message] -> [Tool] -> IO (Either LLMError AssistantMessage)
  , providerCompleteStreaming :: [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (ConduitT () StreamEvent IO ())
  }

-- | Get human-readable provider name
providerName :: Provider -> Text
providerName p = case providerType p of
  OpenAI    -> "OpenAI"
  Anthropic -> "Anthropic"
  Google    -> "Google"
  Mistral   -> "Mistral"
  Copilot   -> "GitHub Copilot"

-- | Send messages and get response (non-streaming)
complete :: Provider -> [Message] -> [Tool] -> IO (Either LLMError AssistantMessage)
complete = providerComplete

-- | Send messages with streaming callback
completeStreaming :: Provider -> [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (ConduitT () StreamEvent IO ())
completeStreaming = providerCompleteStreaming
