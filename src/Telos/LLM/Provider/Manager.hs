

module Telos.LLM.Provider.Manager
  ( createProvider
  , createProviderFromConfig
  , providerTypeToKey
  ) where

import           Control.Lens                 ( (^.) )

import qualified Data.Map.Strict              as Map

import           Network.HTTP.Client          ( Manager )

import           Relude

import           Telos.Config.Types           ( ProviderConfig, TelosConfig, tcModel, tcProviders )
import           Telos.Core.Error             ( AppError(..), LLMError(..) )
import qualified Telos.LLM.Provider.Anthropic as Anthropic
import qualified Telos.LLM.Provider.Google    as Google
import qualified Telos.LLM.Provider.Mistral   as Mistral
import qualified Telos.LLM.Provider.OpenAI    as OpenAI
import qualified Telos.LLM.Provider.Types     as Types
import           Telos.LLM.Provider.Types     ( Provider(..), ProviderType(..) )
import qualified Telos.LLM.Provider.Zai       as Zai

-- | Helper to convert LLMError to AppError
llmToAppError :: LLMError -> AppError
llmToAppError (LLMProviderNotConfigured t) = AppConfigError t
llmToAppError (LLMInvalidResponse t) = AppLLMError (LLMInvalidResponse t)
llmToAppError e = AppLLMError e

-- | Create provider from parsed type and config with HTTP manager
createProvider
  :: ProviderType -> Text -> ProviderConfig -> Manager -> IO (Either AppError Provider)
createProvider pType model config mgr = case pType of
  OpenAI    -> do
    result <- OpenAI.newProvider model config mgr
    pure $ either (Left . llmToAppError) Right result
  Anthropic -> do
    result <- Anthropic.newProvider model config mgr
    pure $ either (Left . llmToAppError) Right result
  Google    -> do
    result <- Google.newProvider model config mgr
    pure $ either (Left . llmToAppError) Right result
  Mistral   -> do
    result <- Mistral.newProvider model config mgr
    pure $ either (Left . llmToAppError) Right result
  Zai       -> do
    result <- Zai.newProvider model config mgr
    pure $ either (Left . llmToAppError) Right result
  Copilot   -> pure
    $ Left
    $ AppConfigError
      "Copilot provider requires special auth flow, use createCopilotProvider instead"

-- | Create provider from TelosConfig
-- Parses model string and routes to appropriate provider
-- For Copilot, returns error - use createCopilotProvider directly
createProviderFromConfig :: TelosConfig -> Manager -> IO (Either AppError Provider)
createProviderFromConfig config mgr = do
  let ( pType, modelName ) = Types.parseModelString (config ^. tcModel)

  case pType of
    Copilot -> pure
      $ Left
      $ AppConfigError
        "Copilot provider requires special auth flow, use createCopilotProvider directly"

    _       -> do
      -- Look up provider config
      case Map.lookup (providerTypeToKey pType) (config ^. tcProviders) of
        Nothing -> pure
          $ Left
          $ AppConfigError
          $ "Provider " <> show pType <> " selected but no config found"
        Just providerConfig -> createProvider pType modelName providerConfig mgr

-- | Convert ProviderType to config map key
providerTypeToKey :: ProviderType -> Text
providerTypeToKey OpenAI    = "openai"
providerTypeToKey Anthropic = "anthropic"
providerTypeToKey Google    = "google"
providerTypeToKey Mistral   = "mistral"
providerTypeToKey Zai       = "zai"
providerTypeToKey Copilot   = "copilot"
