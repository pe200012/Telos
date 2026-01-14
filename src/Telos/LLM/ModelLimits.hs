{-# LANGUAGE OverloadedStrings #-}

-- | Model context length lookup table
--
-- Since LLM APIs don't expose context window sizes, we maintain a hardcoded
-- lookup table. This is standard practice (OpenAI docs, LangChain, etc.).
module Telos.LLM.ModelLimits
  ( ModelLimits(..)
  , getModelLimits
  , getContextLength
  , defaultContextLength
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import           Relude

-- | Model limits information
data ModelLimits
  = ModelLimits { mlContextLength :: !Int  -- ^ Max context window in tokens
                , mlMaxOutput     :: !Int  -- ^ Max output tokens
                }
  deriving stock ( Eq, Show, Generic )

-- | Default context length for unknown models (conservative estimate)
defaultContextLength :: Int
defaultContextLength = 8192

-- | Default model limits for unknown models
defaultLimits :: ModelLimits
defaultLimits = ModelLimits { mlContextLength = defaultContextLength, mlMaxOutput = 4096 }

-- | Get model limits, with fuzzy matching for model name variants
getModelLimits :: Text -> ModelLimits
getModelLimits modelName = fromMaybe defaultLimits $ findModel normalizedName
  where
    normalizedName    = T.toLower modelName

    findModel name
      =
      -- Try exact match first
      Map.lookup name modelLimitsTable
      -- Then try prefix matching for versioned models (e.g., "gpt-4-0613")
      <|> findByPrefix name

    findByPrefix name
      = listToMaybe
        [ limits
        | ( prefix, limits ) <- Map.toList modelLimitsTable
        , prefix `T.isPrefixOf` name || name `T.isPrefixOf` prefix
        ]

-- | Get just the context length for a model
getContextLength :: Text -> Int
getContextLength = mlContextLength . getModelLimits

-- | Lookup table for known models (updated January 2025)
-- Sources:
-- - OpenAI: https://platform.openai.com/docs/models
-- - Anthropic: https://docs.anthropic.com/en/docs/about-claude/models
-- - Google: https://ai.google.dev/gemini-api/docs/models
-- Note: Copilot exposes models from various providers with its own context limits
modelLimitsTable :: Map Text ModelLimits
modelLimitsTable
  = Map.fromList
    [ -- ==========================================================================
      -- OpenAI Models
      -- ==========================================================================

      -- GPT-5.x family (via Copilot)
      ( "gpt-5.2", ModelLimits 128000 64000 )
    , ( "gpt-5.1-codex-mini", ModelLimits 128000 100000 )
    , ( "gpt-5.1-codex-max", ModelLimits 128000 128000 )
    , ( "gpt-5.1-codex", ModelLimits 128000 128000 )
    , ( "gpt-5.1", ModelLimits 128000 128000 )
    , ( "gpt-5-mini", ModelLimits 128000 64000 )
    , ( "gpt-5-codex", ModelLimits 128000 128000 )
    , ( "gpt-5", ModelLimits 128000 128000 )
      -- GPT-4.1 family
    , ( "gpt-4.1", ModelLimits 128000 16384 )
    , ( "gpt-4.1-mini", ModelLimits 128000 16384 )
    , ( "gpt-4.1-nano", ModelLimits 128000 16384 )
      -- GPT-4o family
    , ( "gpt-4o", ModelLimits 64000 16384 )  -- Copilot limit
    , ( "gpt-4o-mini", ModelLimits 128000 16384 )
    , ( "gpt-4o-2024-05-13", ModelLimits 128000 16384 )
    , ( "gpt-4o-2024-08-06", ModelLimits 128000 16384 )
    , ( "gpt-4o-2024-11-20", ModelLimits 128000 16384 )
      -- GPT-4 Turbo (128k context)
    , ( "gpt-4-turbo", ModelLimits 128000 4096 )
    , ( "gpt-4-turbo-preview", ModelLimits 128000 4096 )
    , ( "gpt-4-1106-preview", ModelLimits 128000 4096 )
    , ( "gpt-4-0125-preview", ModelLimits 128000 4096 )
    , ( "gpt-4-vision-preview", ModelLimits 128000 4096 )
      -- GPT-4 base (8k/32k context)
    , ( "gpt-4", ModelLimits 8192 8192 )
    , ( "gpt-4-32k", ModelLimits 32768 32768 )
      -- GPT-3.5 family
    , ( "gpt-3.5-turbo", ModelLimits 16385 4096 )
    , ( "gpt-3.5-turbo-16k", ModelLimits 16385 4096 )
    , ( "gpt-3.5-turbo-1106", ModelLimits 16385 4096 )
    , ( "gpt-3.5-turbo-0125", ModelLimits 16385 4096 )
      -- o-series reasoning models
    , ( "o1", ModelLimits 200000 100000 )
    , ( "o1-pro", ModelLimits 200000 100000 )
    , ( "o1-preview", ModelLimits 128000 32768 )
    , ( "o1-mini", ModelLimits 128000 65536 )
    , ( "o3", ModelLimits 128000 16384 )  -- Copilot limit
    , ( "o3-mini", ModelLimits 128000 65536 )  -- Copilot limit
    , ( "o4-mini", ModelLimits 128000 65536 )  -- Copilot limit
      -- ==========================================================================
      -- Anthropic Claude Models
      -- ==========================================================================

      -- Claude 4.x family (via Copilot)
    , ( "claude-opus-4.5", ModelLimits 128000 16000 )
    , ( "claude-opus-41", ModelLimits 80000 16000 )
    , ( "claude-opus-4", ModelLimits 80000 16000 )
    , ( "claude-sonnet-4.5", ModelLimits 128000 16000 )
    , ( "claude-sonnet-4", ModelLimits 128000 16000 )
    , ( "claude-haiku-4.5", ModelLimits 128000 16000 )
      -- Claude 3.7 family
    , ( "claude-3.7-sonnet-thought", ModelLimits 200000 16384 )
    , ( "claude-3.7-sonnet", ModelLimits 200000 16384 )
    , ( "claude-3-7-sonnet", ModelLimits 200000 16384 )
      -- Claude 3.5 family
    , ( "claude-3.5-sonnet", ModelLimits 90000 8192 )  -- Copilot limit
    , ( "claude-3-5-sonnet", ModelLimits 90000 8192 )  -- Copilot limit
    , ( "claude-3-5-sonnet-20240620", ModelLimits 200000 8192 )
    , ( "claude-3-5-sonnet-20241022", ModelLimits 200000 8192 )
    , ( "claude-3.5-haiku", ModelLimits 200000 8192 )
    , ( "claude-3-5-haiku", ModelLimits 200000 8192 )
    , ( "claude-3-5-haiku-20241022", ModelLimits 200000 8192 )
      -- Claude 3 family
    , ( "claude-3-opus", ModelLimits 200000 4096 )
    , ( "claude-3-opus-20240229", ModelLimits 200000 4096 )
    , ( "claude-3-sonnet", ModelLimits 200000 4096 )
    , ( "claude-3-sonnet-20240229", ModelLimits 200000 4096 )
    , ( "claude-3-haiku", ModelLimits 200000 4096 )
    , ( "claude-3-haiku-20240307", ModelLimits 200000 4096 )
      -- ==========================================================================
      -- Google Models
      -- ==========================================================================

      -- Gemini 3.x family (via Copilot)
    , ( "gemini-3-pro-preview", ModelLimits 128000 64000 )
    , ( "gemini-3-flash-preview", ModelLimits 128000 64000 )
      -- Gemini 2.5 family
    , ( "gemini-2.5-pro", ModelLimits 128000 64000 )  -- Copilot limit
    , ( "gemini-2.5-pro-preview", ModelLimits 1048000 64000 )
    , ( "gemini-2.5-flash", ModelLimits 1048000 64000 )
    , ( "gemini-2.5-flash-preview", ModelLimits 1048000 64000 )
      -- Gemini 2.0 family
    , ( "gemini-2.0-flash-001", ModelLimits 1000000 8192 )  -- Copilot
    , ( "gemini-2.0-flash", ModelLimits 1048000 8192 )
    , ( "gemini-2.0-flash-exp", ModelLimits 1048000 8192 )
    , ( "gemini-2.0-pro", ModelLimits 1048000 8192 )
      -- Gemini 1.5 family
    , ( "gemini-1.5-pro", ModelLimits 2097152 8192 )
    , ( "gemini-1.5-flash", ModelLimits 1048576 8192 )
    , ( "gemini-1.0-pro", ModelLimits 32760 8192 )
    , ( "gemini-pro", ModelLimits 32760 8192 )
      -- Gemma (self-hosted)
    , ( "gemma-3", ModelLimits 128000 8192 )
      -- ==========================================================================
      -- xAI Grok Models
      -- ==========================================================================
    , ( "grok-code-fast-1", ModelLimits 128000 64000 )
    , ( "grok-code-fast", ModelLimits 128000 64000 )
    , ( "grok-2", ModelLimits 131072 8192 )
    , ( "grok-beta", ModelLimits 131072 8192 )
      -- ==========================================================================
      -- DeepSeek Models
      -- ==========================================================================
    , ( "deepseek-chat", ModelLimits 64000 8192 )
    , ( "deepseek-v3", ModelLimits 64000 8192 )
    , ( "deepseek-reasoner", ModelLimits 64000 8192 )
    , ( "deepseek-r1", ModelLimits 64000 8192 )
    , ( "deepseek-coder", ModelLimits 64000 8192 )
      -- ==========================================================================
      -- Alibaba Qwen Models
      -- ==========================================================================
    , ( "qwen2.5-coder-32b", ModelLimits 131072 8192 )
    , ( "qwen2.5-72b-instruct", ModelLimits 131072 8192 )
    , ( "qwen2.5-72b", ModelLimits 131072 8192 )
    , ( "qwen2.5-3b", ModelLimits 32000 8192 )
    , ( "qwq", ModelLimits 32000 8192 )
    , ( "qwen-turbo", ModelLimits 131072 8192 )
    , ( "qwen-plus", ModelLimits 131072 8192 )
    , ( "qwen-max", ModelLimits 32768 8192 )
      -- ==========================================================================
      -- Mistral Models
      -- ==========================================================================
    , ( "mistral-7b-instruct", ModelLimits 32000 4096 )
    , ( "mistral-medium", ModelLimits 32000 4096 )
    , ( "mistral-small", ModelLimits 32000 4096 )
    , ( "mistral-large", ModelLimits 32000 4096 )
    , ( "mistral-nemo", ModelLimits 128000 4096 )
    , ( "mixtral-8x7b", ModelLimits 32000 4096 )
    , ( "mixtral-8x22b", ModelLimits 65536 4096 )
    , ( "codestral", ModelLimits 32000 4096 )
      -- ==========================================================================
      -- Meta Llama Models
      -- ==========================================================================
    , ( "llama3.3:70b", ModelLimits 131072 2048 )
    , ( "llama-3.3-70b", ModelLimits 131072 2048 )
    , ( "llama-3.2-90b", ModelLimits 128000 8192 )
    , ( "llama-3.1-405b", ModelLimits 128000 8192 )
    , ( "llama-3.1-70b", ModelLimits 128000 8192 )
    , ( "llama-3.1-8b", ModelLimits 128000 8192 )
      -- ==========================================================================
      -- Microsoft Phi Models
      -- ==========================================================================
    , ( "phi4", ModelLimits 16000 16000 )
    , ( "phi-4", ModelLimits 16000 16000 )
      -- ==========================================================================
      -- Z.AI Models (Zhipu AI / BigModel)
      -- ==========================================================================
    , ( "glm-4-plus", ModelLimits 200000 128000 ) -- GLM-4.7
    , ( "glm-4.7", ModelLimits 200000 128000 )
    , ( "glm-4.7-thinking", ModelLimits 200000 64000 )
    , ( "glm-4.6", ModelLimits 200000 128000 )
    , ( "glm-4.6v", ModelLimits 128000 32000 )  -- aka glm-4v-plus
    , ( "glm-4v-plus", ModelLimits 128000 32000 )
    , ( "glm-4-air", ModelLimits 128000 96000 )  -- GLM-4.5-Air
    , ( "glm-4-airx", ModelLimits 128000 96000 )  -- GLM-4.5-AirX
    , ( "glm-4-long", ModelLimits 1024000 4000 )  -- 1M Context
    , ( "glm-4-flash", ModelLimits 128000 96000 )  -- GLM-4.5-Flash
    , ( "glm-4-flashx", ModelLimits 128000 16000 )  -- GLM-4-FlashX-250414
    , ( "glm-4v", ModelLimits 16000 1000 )    -- GLM-4V-Flash
    , ( "glm-4-0520", ModelLimits 128000 4000 )
    , ( "glm-4-alltools", ModelLimits 128000 64000 )
    , ( "glm-3-turbo", ModelLimits 128000 64000 )
    , ( "glm-3v", ModelLimits 128000 64000 )
    ]
