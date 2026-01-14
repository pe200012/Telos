# Design Log #013: LLM Provider Abstraction Layer

## Background

Telos currently hardcodes GitHub Copilot as the only LLM provider. The configuration system (Tasks 1-3, 8 complete) now supports:
- Model specification in `telos.json` (e.g., `"model": "copilot/gpt-4o"`)
- Provider-specific API keys in config (OpenAI, Anthropic, Google, Mistral)
- Environment variable overrides (`TELOS_MODEL`, `OPENAI_API_KEY`, etc.)

However, the application still only works with Copilot. We need a provider abstraction layer to support multiple LLM providers dynamically based on the model string format.

---

## Problem

**Current limitations**:
1. Hardcoded `CopilotClient` in `Main.hs` and `App.hs`
2. No way to use OpenAI, Anthropic, Google, or Mistral models
3. Config has provider settings (`ProviderConfig`) but they're unused
4. Model string format `"provider/model"` is parsed but not acted upon

**What needs to happen**:
- Parse model string (e.g., `"openai/gpt-4"` → provider: "openai", model: "gpt-4")
- Route to appropriate provider client based on prefix
- Each provider implements same interface for chat completion
- Support both streaming and non-streaming modes
- Handle provider-specific authentication (API keys, tokens)

---

## Questions and Answers

**Q1: What should the provider interface look like?**
A: Use a typeclass approach for polymorphism:
```haskell
class LLMProvider p where
  -- Send messages and get response
  complete :: p -> [Message] -> [Tool] -> IO (Either AppError Message)
  
  -- Streaming version
  completeStreaming :: p -> [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (Either AppError Message)
  
  -- Get provider name
  providerName :: p -> Text
```

**Q2: How do we parse the model string?**
A: Format: `"provider/model"` or just `"model"` (defaults to copilot)
```haskell
parseModelString :: Text -> (Text, Text)
parseModelString str = case T.splitOn "/" str of
  [provider, model] -> (provider, model)
  [model]           -> ("copilot", model)  -- Default to copilot
  _                 -> ("copilot", "gpt-4o")  -- Fallback
```

**Q3: Where should provider config come from?**
A: Priority chain (same as TelosConfig):
1. TelosConfig `providers` map (from `telos.json`)
2. Environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc.)
3. Error if provider selected but no config/key available

**Q4: Should we support multiple providers simultaneously?**
A: Not in Phase 1. Single provider per session based on `model` config.
Future: Could support provider switching via REPL commands.

**Q5: How do we handle provider-specific features?**
A: 
- Use common subset for interface (messages, tools, streaming)
- Provider-specific options go in `ProviderConfig` (base_url, timeout, headers)
- Ignore features not supported by a provider (log warning)

**Q6: What about existing Polysemy LLM effect?**
A: Keep the effect interface unchanged. Provider abstraction sits between effect interpreter and actual HTTP clients.

**Q7: Should Copilot remain special?**
A: Yes, for now:
- Copilot uses GitHub auth flow (different from API key)
- Keep `CopilotAuth` and existing client
- Treat as one provider among many in the abstraction

---

## Design

### Architecture Overview

```
┌─────────────────────────────────────────────┐
│  Application Layer (Main.hs, App.hs)       │
│  - Loads TelosConfig                        │
│  - Parses model string → (provider, model)  │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│  Provider Manager                           │
│  - createProvider :: ProviderType → Config → IO Provider
│  - Routes to correct provider impl          │
└──────────────────┬──────────────────────────┘
                   │
        ┌──────────┴──────────┐
        │                     │
┌───────▼────────┐  ┌─────────▼─────────┐
│  LLMProvider   │  │  Concrete         │
│  Typeclass     │  │  Implementations  │
│                │  │  - OpenAI         │
│  complete      │  │  - Anthropic      │
│  streaming     │  │  - Google         │
│  providerName  │  │  - Mistral        │
└────────────────┘  │  - Copilot        │
                    └───────────────────┘
```

### Module Structure

```
src/Telos/LLM/
├── Provider/
│   ├── Types.hs          -- Provider typeclass, common types
│   ├── Manager.hs        -- Provider creation and routing
│   ├── OpenAI.hs         -- OpenAI implementation
│   ├── Anthropic.hs      -- Anthropic implementation
│   ├── Google.hs         -- Google Gemini implementation
│   ├── Mistral.hs        -- Mistral implementation
│   └── Copilot.hs        -- Wrapper for existing Copilot client
└── Copilot/              -- Existing Copilot code (unchanged)
    ├── Auth.hs
    ├── Client.hs
    └── ...
```

### Core Types

```haskell
-- src/Telos/LLM/Provider/Types.hs
module Telos.LLM.Provider.Types where

import Telos.Core.Types (Message, Tool, StreamEvent)
import Telos.Core.Error (AppError)

-- Provider identification
data ProviderType
  = OpenAI
  | Anthropic
  | Google
  | Mistral
  | Copilot
  deriving (Eq, Show, Enum, Bounded)

-- Parse provider from string
parseProvider :: Text -> Maybe ProviderType
parseProvider "openai"    = Just OpenAI
parseProvider "anthropic" = Just Anthropic
parseProvider "google"    = Just Google
parseProvider "mistral"   = Just Mistral
parseProvider "copilot"   = Just Copilot
parseProvider _           = Nothing

-- Parse model string into (provider, model)
parseModelString :: Text -> (ProviderType, Text)
parseModelString str = case T.splitOn "/" str of
  [providerStr, model] -> case parseProvider providerStr of
    Just provider -> (provider, model)
    Nothing       -> (Copilot, str)  -- Unknown provider → default
  [model] -> (Copilot, model)  -- No provider prefix → default
  _       -> (Copilot, "gpt-4o")  -- Invalid format → fallback

-- Unified provider interface
data Provider = Provider
  { providerType :: ProviderType
  , providerModel :: Text
  , providerComplete :: [Message] -> [Tool] -> IO (Either AppError Message)
  , providerCompleteStreaming :: [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (Either AppError Message)
  }

-- Convenience functions
providerName :: Provider -> Text
providerName p = case providerType p of
  OpenAI    -> "OpenAI"
  Anthropic -> "Anthropic"
  Google    -> "Google"
  Mistral   -> "Mistral"
  Copilot   -> "GitHub Copilot"

complete :: Provider -> [Message] -> [Tool] -> IO (Either AppError Message)
complete = providerComplete

completeStreaming :: Provider -> [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (Either AppError Message)
completeStreaming = providerCompleteStreaming
```

### Provider Manager

```haskell
-- src/Telos/LLM/Provider/Manager.hs
module Telos.LLM.Provider.Manager
  ( createProvider
  , createProviderFromConfig
  ) where

import Telos.Config.Types (TelosConfig, ProviderConfig, tcProviders, tcModel)
import Telos.LLM.Provider.Types
import qualified Telos.LLM.Provider.OpenAI as OpenAI
import qualified Telos.LLM.Provider.Anthropic as Anthropic
-- ... other imports

-- Create provider from parsed components
createProvider 
  :: ProviderType 
  -> Text  -- model name
  -> ProviderConfig 
  -> Manager  -- HTTP manager
  -> IO (Either AppError Provider)
createProvider providerType model config manager = case providerType of
  OpenAI    -> OpenAI.newProvider model config manager
  Anthropic -> Anthropic.newProvider model config manager
  Google    -> Google.newProvider model config manager
  Mistral   -> Mistral.newProvider model config manager
  Copilot   -> error "Copilot requires special auth flow, use createCopilotProvider"

-- Create provider from TelosConfig (convenience)
createProviderFromConfig 
  :: TelosConfig 
  -> CopilotAuth  -- Only needed for Copilot
  -> Manager 
  -> IO (Either AppError Provider)
createProviderFromConfig config copilotAuth manager = do
  let modelStr = config ^. tcModel
      (providerType, modelName) = parseModelString modelStr
  
  case providerType of
    Copilot -> do
      -- Use existing Copilot client with auth
      pure $ Right $ Provider
        { providerType = Copilot
        , providerModel = modelName
        , providerComplete = \msgs tools -> 
            -- Delegate to existing CopilotClient
            runCopilotComplete copilotAuth manager modelName msgs tools
        , providerCompleteStreaming = \msgs tools callback ->
            runCopilotStreaming copilotAuth manager modelName msgs tools callback
        }
    
    _ -> do
      -- Look up provider config
      let providers = config ^. tcProviders
      case Map.lookup (providerTypeToKey providerType) providers of
        Nothing -> pure $ Left $ ConfigError $ 
          "Provider " <> show providerType <> " selected but no config found"
        Just providerConfig -> 
          createProvider providerType modelName providerConfig manager

providerTypeToKey :: ProviderType -> Text
providerTypeToKey OpenAI    = "openai"
providerTypeToKey Anthropic = "anthropic"
providerTypeToKey Google    = "google"
providerTypeToKey Mistral   = "mistral"
providerTypeToKey Copilot   = "copilot"
```

### Example Provider Implementation (OpenAI)

```haskell
-- src/Telos/LLM/Provider/OpenAI.hs
module Telos.LLM.Provider.OpenAI (newProvider) where

import Telos.Config.Types (ProviderConfig, pcApiKey, pcBaseUrl, pcTimeout)
import Telos.LLM.Provider.Types (Provider(..), ProviderType(..))
import Telos.Core.Types (Message, Tool, StreamEvent)
import Network.HTTP.Client (Manager)

data OpenAIClient = OpenAIClient
  { _oaiManager :: Manager
  , _oaiApiKey :: Text
  , _oaiBaseUrl :: Text
  , _oaiModel :: Text
  }

newProvider :: Text -> ProviderConfig -> Manager -> IO (Either AppError Provider)
newProvider model config manager = do
  case config ^. pcApiKey of
    Nothing -> pure $ Left $ ConfigError "OpenAI API key not configured"
    Just apiKey -> do
      let client = OpenAIClient
            { _oaiManager = manager
            , _oaiApiKey = apiKey
            , _oaiBaseUrl = fromMaybe "https://api.openai.com/v1" (config ^. pcBaseUrl)
            , _oaiModel = model
            }
      
      pure $ Right $ Provider
        { providerType = OpenAI
        , providerModel = model
        , providerComplete = openaiComplete client
        , providerCompleteStreaming = openaiCompleteStreaming client
        }

openaiComplete :: OpenAIClient -> [Message] -> [Tool] -> IO (Either AppError Message)
openaiComplete client msgs tools = do
  -- Convert Telos Message → OpenAI format
  let openaiMessages = map convertMessage msgs
      openaiTools = map convertTool tools
  
  -- Make HTTP request to OpenAI API
  -- ... implementation ...
  
  -- Convert OpenAI response → Telos Message
  pure $ Right $ convertResponse response

openaiCompleteStreaming :: OpenAIClient -> [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (Either AppError Message)
openaiCompleteStreaming client msgs tools callback = do
  -- Similar to openaiComplete but with SSE streaming
  -- ... implementation ...
  pure $ Right finalMessage
```

---

## Implementation Plan

### Phase 1: Core Infrastructure

#### 1.1 Create `src/Telos/LLM/Provider/Types.hs`
- Define `ProviderType` enum
- Implement `parseModelString` and `parseProvider`
- Define `Provider` record with function fields
- Add helper functions (`providerName`, `complete`, etc.)

#### 1.2 Create `src/Telos/LLM/Provider/Manager.hs`
- Implement `createProvider` routing logic
- Implement `createProviderFromConfig` convenience function
- Add Copilot special case handling

#### 1.3 Create Copilot wrapper `src/Telos/LLM/Provider/Copilot.hs`
- Wrap existing `CopilotClient` into `Provider` interface
- Reuse existing auth and HTTP logic
- No changes to existing Copilot code

### Phase 2: Provider Implementations

#### 2.1 OpenAI Provider (`src/Telos/LLM/Provider/OpenAI.hs`)
- Implement `newProvider` with API key validation
- Implement `openaiComplete` (non-streaming)
- Implement `openaiCompleteStreaming` (SSE)
- Message/Tool format conversion (Telos ↔ OpenAI)

#### 2.2 Anthropic Provider (`src/Telos/LLM/Provider/Anthropic.hs`)
- Similar to OpenAI but with Anthropic API format
- Handle Claude-specific message format differences

#### 2.3 Google Provider (`src/Telos/LLM/Provider/Google.hs`)
- Gemini API integration
- Handle Google-specific auth (API key in URL vs header)

#### 2.4 Mistral Provider (`src/Telos/LLM/Provider/Mistral.hs`)
- Mistral API integration (similar to OpenAI format)

### Phase 3: Integration

#### 3.1 Update `app/Main.hs`
- Replace `CopilotClient` creation with `createProviderFromConfig`
- Pass `Provider` to REPL instead of `CopilotClient`

#### 3.2 Update `src/Telos/App.hs`
- Replace `CopilotClient` parameter with `Provider`
- Update agent loop to use `Provider` interface

#### 3.3 Update `src/Telos/Effect/LLM/Copilot.hs` (Polysemy interpreter)
- Rename to `src/Telos/Effect/LLM/Provider.hs`
- Change from Copilot-specific to generic Provider
- Update interpreter to use `Provider` interface

### Phase 4: Testing & Validation

#### 4.1 Unit tests for provider parsing
- Test `parseModelString` with various inputs
- Test `parseProvider` with valid/invalid strings

#### 4.2 Integration tests
- Test each provider with real API (or mocks)
- Test provider switching via config

#### 4.3 Manual testing
- Test with each provider in REPL
- Verify streaming works for all providers

---

## Examples

### Example 1: Config with Multiple Providers

```json
{
  "model": "openai/gpt-4",
  "providers": {
    "openai": {
      "api_key": "sk-...",
      "base_url": "https://api.openai.com/v1",
      "timeout": 60000
    },
    "anthropic": {
      "api_key": "sk-ant-...",
      "timeout": 90000
    },
    "copilot": {}
  }
}
```

### Example 2: Switching Providers via Config

```bash
# Use OpenAI
echo '{"model": "openai/gpt-4"}' > telos.json

# Use Anthropic
echo '{"model": "anthropic/claude-3-opus"}' > telos.json

# Use Copilot (default)
echo '{"model": "copilot/gpt-4o"}' > telos.json
# OR just:
echo '{"model": "gpt-4o"}' > telos.json
```

### Example 3: Environment Variable Override

```bash
# Override model
TELOS_MODEL=openai/gpt-4 telos

# Provide API key via env
OPENAI_API_KEY=sk-... telos
```

### Example 4: Usage in Code

```haskell
-- Main.hs
main = do
  telosConfig <- loadTelosConfig
  httpManager <- newManager tlsManagerSettings
  
  -- Create provider based on config
  providerResult <- case parseModelString (telosConfig ^. tcModel) of
    (Copilot, modelName) -> do
      auth <- newCopilotAuth httpManager
      pure $ Right $ createCopilotProvider auth httpManager modelName
    (providerType, modelName) -> 
      createProviderFromConfig telosConfig undefined httpManager
  
  provider <- case providerResult of
    Left err -> die $ "Failed to create provider: " <> show err
    Right p -> pure p
  
  -- Use provider
  result <- complete provider messages tools
```

---

## Trade-offs

### Benefits

✅ **Multi-provider support**: Users can choose OpenAI, Anthropic, Google, Mistral, or Copilot
✅ **Config-driven**: No code changes needed to switch providers
✅ **Extensible**: Easy to add new providers (implement interface)
✅ **Testable**: Provider interface enables mocking for tests
✅ **Unified interface**: Same API for all providers (streaming, tools, messages)

### Costs

❌ **Complexity**: Adds abstraction layer and multiple implementations
❌ **Provider differences**: Each API has quirks (message format, tool calling, streaming)
❌ **Maintenance**: Need to keep up with API changes from multiple vendors
❌ **Error handling**: Different providers have different error formats
❌ **Testing burden**: Need to test with each provider

### Decisions

1. **Use record-of-functions pattern**: Simpler than typeclass for dynamic dispatch
2. **Keep Copilot special**: Auth flow is unique, don't force it into generic mold
3. **Parse model string at startup**: Fail fast if provider unsupported
4. **Phase implementation**: Copilot first, then one provider at a time
5. **Provider-specific configs**: Allow base_url, timeout, headers per provider

---

## Implementation Results

*Phase 1-4 pending implementation*

---

## Deviations from Original Design

*None yet - design phase complete*
