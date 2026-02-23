{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Provider.Spec
  ( ProviderSpec
  , HasApiKeyEnv(..)
  , HasAuthHeader(..)
  , HasAuthPrefix(..)
  , HasDefaultBaseUrl(..)
  , HasDefaultModel(..)
  , HasDefaultTemperature(..)
  , HasSupportsToolCalls(..)
  ) where

import           Control.Lens ( makeFieldsNoPrefix )

import           Data.Aeson   ( FromJSON(parseJSON), (.:), withObject )

import           Relude

data ProviderSpec
  = ProviderSpec
      { _apiKeyEnv           :: Text
      , _authHeader          :: Text
      , _authPrefix          :: Text
      , _defaultBaseUrl      :: Text
      , _defaultModel        :: Text
      , _defaultTemperature  :: Double
      , _supportsToolCalls   :: Bool
      }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ProviderSpec

instance FromJSON ProviderSpec where
  parseJSON = withObject "ProviderSpec" $ \obj -> ProviderSpec
    <$> obj .: "api_key_env"
    <*> obj .: "auth_header"
    <*> obj .: "auth_prefix"
    <*> obj .: "default_base_url"
    <*> obj .: "default_model"
    <*> obj .: "default_temperature"
    <*> obj .: "supports_tool_calls"
