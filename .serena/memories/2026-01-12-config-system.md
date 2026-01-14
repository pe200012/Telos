# Telos Configuration System Implementation Plan

**Date**: 2026-01-12
**Status**: Draft

## Goal

Implement a comprehensive configuration system for Telos, inspired by OpenCode's `opencode.json`. This enables users to configure providers, models, permissions, MCP servers, and DCP settings via a config file.

## Architecture

```
~/.config/telos/telos.json (global)
        ↓ merged with
./telos.json (project-local, higher priority)
        ↓ merged with
Environment variables (highest priority)
        ↓
TelosConfig (runtime)
```

### Config Loading Priority (lowest to highest)
1. Default values (hardcoded)
2. Global config: `~/.config/telos/telos.json`
3. Project config: `./telos.json` (findUp from CWD)
4. Environment variables: `TELOS_MODEL`, `TELOS_PROVIDER`, etc.

## Tech Stack

- **Format**: JSON (via `aeson` package)
- **Schema**: Haskell ADTs with `FromJSON`/`ToJSON` instances
- **Lenses**: `microlens-th` for field access
- **Validation**: Pure functions returning `Either ConfigError TelosConfig`

## Current State Analysis

**Existing Config**:
- `CLI/Config.hs`: `CliConfig` with `model`, `maxIterations`, `mcpServers`, `pruneConfig`
- `Agent/Config.hs`: `AgentConfig`, `MCPServerConfig`
- Config loaded from `~/.config/telos/config.json`

**Gap**: No provider config, no permission system, no project-local config, JSON only (already!).

---

## Task 1: Create Core Config Types

**Files**: `src/Telos/Config/Types.hs` (new)

### Steps

1. Create new module `Telos.Config.Types`
2. Define `TelosConfig` as the root config type:
   ```haskell
   data TelosConfig = TelosConfig
     { _tcModel           :: Text                      -- "provider/model"
     , _tcSmallModel      :: Maybe Text                -- for summaries
     , _tcProviders       :: Map Text ProviderConfig
     , _tcMcp             :: Map Text McpConfig
     , _tcPermissions     :: Map Text Permission
     , _tcInstructions    :: [FilePath]
     , _tcCompaction      :: CompactionConfig
     , _tcMaxIterations   :: Int
     , _tcStreamingEnabled:: Bool
     , _tcLogLevel        :: LogLevel
     }
   ```
3. Define `ProviderConfig`:
   ```haskell
   data ProviderConfig = ProviderConfig
     { _pcApiKey    :: Maybe Text
     , _pcBaseURL   :: Maybe Text
     , _pcTimeout   :: Maybe Int  -- seconds
     }
   ```
4. Define `McpConfig` (local and remote):
   ```haskell
   data McpConfig
     = McpLocal
       { _mcName        :: Text
       , _mcCommand     :: Text
       , _mcArgs        :: [Text]
       , _mcEnv         :: Map Text Text
       , _mcEnabled     :: Bool
       , _mcTimeout     :: Maybe Int
       }
     | McpRemote
       { _mcName        :: Text
       , _mcUrl         :: Text
       , _mcHeaders     :: Map Text Text
       , _mcEnabled     :: Bool
       , _mcTimeout     :: Maybe Int
       }
   ```
5. Define `Permission`:
   ```haskell
   data Permission = Ask | Allow | Deny
   ```
6. Define `CompactionConfig`:
   ```haskell
   data CompactionConfig = CompactionConfig
     { _ccAuto       :: Bool    -- auto-compact when threshold reached
     , _ccPrune      :: Bool    -- enable DCP
     , _ccThreshold  :: Double  -- trigger at % of context (e.g., 0.8)
     }
   ```
7. Define `LogLevel`:
   ```haskell
   data LogLevel = Debug | Info | Warn | Error
   ```
8. Generate lenses with `makeLenses`
9. Implement `FromJSON` instances with sensible defaults
10. Implement `ToJSON` instances
11. Export all types and lenses

---

## Task 2: Config Loading & Merging

**Files**: `src/Telos/Config/Load.hs` (new)

### Steps

1. Create `Telos.Config.Load` module
2. Implement `findProjectConfig :: IO (Maybe FilePath)` using directory traversal
2. Implement `loadJsonConfig :: FilePath -> IO (Either ConfigError TelosConfig)`
4. Implement `mergeConfigs :: TelosConfig -> TelosConfig -> TelosConfig` (right-biased)
5. Implement `applyEnvOverrides :: TelosConfig -> IO TelosConfig`:
   - `TELOS_MODEL` → `_tcModel`
   - `TELOS_LOG_LEVEL` → `_tcLogLevel`
   - `OPENAI_API_KEY` → `_tcProviders.openai._pcApiKey`
   - `ANTHROPIC_API_KEY` → `_tcProviders.anthropic._pcApiKey`
6. Implement `loadConfig :: IO TelosConfig` (main entry point):
   ```haskell
   loadConfig = do
     global <- loadGlobalConfig
     project <- loadProjectConfig
     let merged = mergeConfigs defaultConfig (mergeConfigs global project)
     applyEnvOverrides merged
   ```
7. Implement `defaultConfig :: TelosConfig` with sensible defaults
8. Define `ConfigError` type for validation errors

---

## Task 3: Migrate Existing Config

**Files**: `src/Telos/CLI/Config.hs` (modify), `src/Telos/Agent/Config.hs` (modify)

### Steps

1. Update `CliConfig` to use `TelosConfig` internally or deprecate
2. Update `loadConfig` in `CLI/Config.hs` to delegate to `Config.Load`
3. Add backward compatibility: if `config.json` exists, migrate to `telos.yaml`
4. Update `AgentConfig` to derive from `TelosConfig`:
   ```haskell
   toAgentConfig :: TelosConfig -> AgentConfig
   ```
5. Update `MCPServerConfig` to work with new `McpConfig`
6. Ensure `PruneConfig` is derived from `CompactionConfig`

---

## Task 4: Provider Integration

**Files**: `src/Telos/LLM/Provider.hs` (new), `src/Telos/LLM/Provider/*.hs` (new)

### Steps

1. Create `Telos.LLM.Provider` module with provider abstraction:
   ```haskell
   data Provider = Provider
     { providerName    :: Text
     , providerSend    :: ChatRequest -> IO ChatResponse
     , providerStream  :: ChatRequest -> ConduitT () StreamEvent IO ()
     }
   ```
2. Create `Telos.LLM.Provider.OpenAI` with OpenAI-compatible API
3. Create `Telos.LLM.Provider.Anthropic` with Claude API
4. Create `Telos.LLM.Provider.Copilot` wrapping existing code
5. Implement `selectProvider :: TelosConfig -> Text -> IO Provider`:
   - Parse "provider/model" format
   - Look up provider config
   - Initialize with API key from config or env
6. Update `App.hs` to use provider abstraction

---

## Task 5: Permission System

**Files**: `src/Telos/Config/Permission.hs` (new), `src/Telos/Tool/*.hs` (modify)

### Steps

1. Create `Telos.Config.Permission` module
2. Implement `checkPermission :: TelosConfig -> Text -> IO PermissionResult`:
   ```haskell
   data PermissionResult = Allowed | Denied | NeedsConfirmation
   ```
3. Implement permission prompt for `Ask` case
4. Update tool execution in `Loop.hs` to check permissions before running
5. Add default permissions:
   - `read`, `glob`, `grep` → `Allow`
   - `edit`, `write`, `bash` → `Ask`
   - `webfetch` → `Ask`

---

## Task 6: Instructions Loading

**Files**: `src/Telos/Config/Instructions.hs` (new), `src/Telos/Prompt/System.hs` (modify)

### Steps

1. Create `Telos.Config.Instructions` module
2. Implement `loadInstructions :: TelosConfig -> IO [Text]`:
   - Load from `_tcInstructions` paths
   - Also scan `.telos/` directory for `*.md` files
3. Update system prompt generation to append instructions
4. Support glob patterns in instruction paths

---

## Task 7: REPL Integration

**Files**: `src/Telos/CLI/Repl.hs` (modify)

### Steps

1. Add `/config` command to display current config
2. Add `/config reload` to reload config at runtime
3. Add `/config set <key> <value>` for runtime overrides
4. Update `/model` command to validate against provider config
5. Show provider info in model display

---

## Task 8: Update App Entry Point

**Files**: `src/Telos/App.hs` (modify), `app/Main.hs` (modify)

### Steps

1. Update `runApp` to load `TelosConfig` first
2. Derive `AgentConfig` and `CopilotConfig` from `TelosConfig`
3. Pass config to REPL for runtime access
4. Add `--config` CLI flag to specify config file
5. Add `--model` CLI flag to override model

---

## Task 9: Documentation & Defaults

**Files**: `docs/CONFIG.md` (new), `telos.example.json` (new)

### Steps

1. Create example config file `telos.example.json`
2. Document all config options in `docs/CONFIG.md`
3. Add config section to README

---

## File Summary

| File | Action | Description |
|------|--------|-------------|
| `src/Telos/Config/Types.hs` | Create | Core config types |
| `src/Telos/Config/Load.hs` | Create | Config loading & merging |
| `src/Telos/Config/Permission.hs` | Create | Permission checking |
| `src/Telos/Config/Instructions.hs` | Create | Instructions loading |
| `src/Telos/LLM/Provider.hs` | Create | Provider abstraction |
| `src/Telos/CLI/Config.hs` | Modify | Delegate to new system |
| `src/Telos/Agent/Config.hs` | Modify | Derive from TelosConfig |
| `src/Telos/CLI/Repl.hs` | Modify | /config commands |
| `src/Telos/App.hs` | Modify | Use new config |
| `telos.example.json` | Create | Example config |

---

## Example Config

```json
{
  "model": "anthropic/claude-3.5-sonnet",
  "small_model": "anthropic/claude-3-haiku",
  
  "providers": {
    "openai": {
      "api_key": "${OPENAI_API_KEY}",
      "base_url": "https://api.openai.com/v1"
    },
    "anthropic": {
      "api_key": "${ANTHROPIC_API_KEY}"
    }
  },
  
  "mcp": {
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@anthropic/mcp-filesystem"],
      "env": {
        "HOME": "/home/user"
      }
    }
  },
  
  "permissions": {
    "bash": "ask",
    "edit": "ask",
    "read": "allow",
    "webfetch": "deny"
  },
  
  "compaction": {
    "auto": true,
    "prune": true,
    "threshold": 0.8
  },
  
  "instructions": [
    ".telos/AGENTS.md"
  ],
  
  "max_iterations": 50,
  "streaming_enabled": true,
  "log_level": "info"
}
```

---

## Execution Options

Ready to implement. Options:

1. **Full implementation** - All tasks sequentially
2. **Core only** - Tasks 1-3 (types, loading, migration)
3. **Incremental** - Task by task with verification

Which approach?
