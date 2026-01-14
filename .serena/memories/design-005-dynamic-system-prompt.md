# Design Log #005: Dynamic System Prompt & AGENTS.md Support

## Background

Currently Telos has a simple static system prompt mechanism:
- `AgentConfig._acSystemPrompt :: Maybe Text` - optional static prompt from config
- In `Loop.hs`, if present, prepended to history as `Core.SystemMessage`
- No model-specific prompts, no project-local rules

OpenCode uses a sophisticated layered approach:
1. **Model-specific prompts** - different templates for GPT-4, Claude, Gemini
2. **Environment injection** - working directory, platform, date
3. **AGENTS.md discovery** - recursive upward search for project rules

## Problem

1. Static prompt doesn't adapt to different LLM backends
2. No way to inject project-specific rules (like "use Lens", "no placeholders")
3. No dynamic environment context (cwd, git status, tools available)

## Questions and Answers

**Q1: Should we support multiple LLM backends with different prompts?**
A: For now, Telos only uses GitHub Copilot (GPT-4o). We can start with a single high-quality prompt and add model routing later.

**Q2: Where should prompt templates be stored?**
A: Options:
  - (a) Embedded in Haskell code as multiline strings
  - (b) External `.txt` files loaded at runtime
  - (c) XDG config directory
Recommendation: Start with (a) for simplicity, migrate to (b) later for customization.

**Q3: How should AGENTS.md discovery work?**
A: Recursively search from cwd upward until root, collect all AGENTS.md files, concatenate in order (most specific last).

**Q4: Should we support user-global rules?**
A: Yes, check `~/.config/telos/AGENTS.md` as fallback.

## Design

### Phase 1: Core Types

```haskell
-- src/Telos/Prompt/Types.hs
module Telos.Prompt.Types where

data PromptSection
  = PromptSection
  { _psName    :: Text        -- "identity", "tools", "environment", "rules"
  , _psContent :: Text
  , _psPriority :: Int        -- lower = earlier in final prompt
  }

data SystemPromptConfig
  = SystemPromptConfig
  { _spcWorkingDir   :: FilePath
  , _spcIsGitRepo    :: Bool
  , _spcPlatform     :: Text
  , _spcTools        :: [Tool]     -- available tools for description
  , _spcModelId      :: Text       -- for future model-specific routing
  }
```

### Phase 2: Prompt Builder

```haskell
-- src/Telos/Prompt/Builder.hs
module Telos.Prompt.Builder where

-- | Build complete system prompt from config
buildSystemPrompt :: SystemPromptConfig -> IO Text
buildSystemPrompt cfg = do
  identity    <- pure $ identitySection
  tools       <- pure $ toolsSection (cfg ^. spcTools)
  environment <- environmentSection cfg
  agentRules  <- discoverAgentsRules (cfg ^. spcWorkingDir)
  
  let sections = [identity, tools, environment] <> agentRules
  pure $ assembleSections sections

-- | Core identity prompt (embedded)
identitySection :: PromptSection
identitySection = PromptSection "identity" prompt 0
  where
    prompt = """
    You are Telos, an AI programming assistant specialized in Haskell development.
    
    ## Core Principles
    - Write production-quality code, never use placeholders or TODOs
    - Follow existing codebase patterns and conventions
    - Use Lens for record manipulation when the project uses it
    - Prefer type-safe solutions over runtime checks
    
    ## Workflow
    1. Understand the request fully before acting
    2. Read relevant files before modifying
    3. Make minimal, focused changes
    4. Verify your changes compile (use bash to run stack build)
    """

-- | Generate tools description section
toolsSection :: [Tool] -> PromptSection
toolsSection tools = PromptSection "tools" content 10
  where
    content = "## Available Tools\n\n" <> T.intercalate "\n\n" (map formatTool tools)
    formatTool t = "### " <> (t ^. toolName) <> "\n" <> (t ^. toolDescription)

-- | Generate environment section
environmentSection :: SystemPromptConfig -> IO PromptSection
environmentSection cfg = do
  now <- getCurrentTime
  pure $ PromptSection "environment" content 20
  where
    content = T.unlines
      [ "## Environment"
      , "- Working directory: " <> T.pack (cfg ^. spcWorkingDir)
      , "- Git repository: " <> if cfg ^. spcIsGitRepo then "yes" else "no"
      , "- Platform: " <> (cfg ^. spcPlatform)
      , "- Date: " <> T.pack (show now)
      ]
```

### Phase 3: AGENTS.md Discovery

```haskell
-- src/Telos/Prompt/Discovery.hs
module Telos.Prompt.Discovery where

-- | Discover all AGENTS.md files from cwd upward + global config
discoverAgentsRules :: FilePath -> IO [PromptSection]
discoverAgentsRules startDir = do
  -- 1. Find project AGENTS.md files (from cwd upward)
  projectFiles <- findAgentsFiles startDir
  
  -- 2. Check global config
  globalFile <- getGlobalAgentsFile
  
  -- 3. Read and parse all
  let allFiles = maybeToList globalFile <> projectFiles
  sections <- forM (zip [100..] allFiles) $ \(priority, path) -> do
    content <- TIO.readFile path
    pure $ PromptSection ("rules:" <> T.pack path) content priority
  
  pure sections

-- | Recursively find AGENTS.md files from dir upward
findAgentsFiles :: FilePath -> IO [FilePath]
findAgentsFiles dir = do
  let agentsPath = dir </> "AGENTS.md"
  exists <- doesFileExist agentsPath
  let current = [agentsPath | exists]
  
  let parent = takeDirectory dir
  if parent == dir  -- reached root
    then pure current
    else do
      parentFiles <- findAgentsFiles parent
      pure $ parentFiles <> current  -- parent first, then current (more specific)

-- | Get global AGENTS.md path
getGlobalAgentsFile :: IO (Maybe FilePath)
getGlobalAgentsFile = do
  configDir <- getXdgDirectory XdgConfig "telos"
  let path = configDir </> "AGENTS.md"
  exists <- doesFileExist path
  pure $ if exists then Just path else Nothing
```

### Phase 4: Integration with Agent Loop

```haskell
-- Modify AgentConfig to use builder instead of static text
data AgentConfig = AgentConfig
  { _acPromptConfig :: SystemPromptConfig  -- NEW: replaces acSystemPrompt
  , _acMaxIterations :: Int
  , ...
  }

-- In runAgentLoop, build prompt dynamically
runAgentLoop :: ... -> Sem r AgentResult
runAgentLoop ctx userMessage = do
  -- Build system prompt dynamically
  systemPrompt <- embed $ buildSystemPrompt (ctx ^. ctxConfig . acPromptConfig)
  
  let fullHistory = [Core.SystemMessage systemPrompt] <> ...
```

## Implementation Plan

### Phase 1: Core Infrastructure (1-2 hours)
- [ ] Create `src/Telos/Prompt/Types.hs` with `PromptSection`, `SystemPromptConfig`
- [ ] Create `src/Telos/Prompt/Builder.hs` with `buildSystemPrompt`, `identitySection`
- [ ] Add lenses

### Phase 2: AGENTS.md Discovery (1 hour)
- [ ] Create `src/Telos/Prompt/Discovery.hs` with `discoverAgentsRules`, `findAgentsFiles`
- [ ] Add global config file support

### Phase 3: Integration (1 hour)
- [ ] Update `AgentConfig` to use `SystemPromptConfig`
- [ ] Modify `runAgentLoop` to call `buildSystemPrompt`
- [ ] Update `Repl.hs` to construct `SystemPromptConfig` with current cwd, tools, etc.

### Phase 4: Polish (30 min)
- [ ] Write default identity prompt (Haskell-focused)
- [ ] Add tool descriptions to prompt
- [ ] Test with real interactions

## Examples

### Project AGENTS.md
```markdown
# Project Rules

1. This is a Haskell project using GHC 9.12 and Stack.
2. Use `Lens.Micro` for all record access, never raw field accessors.
3. Run `hlint .` after changes and apply suggestions.
4. Format code with `floskell`.
5. Never use placeholders - deliver complete, working code.
```

### Generated System Prompt (assembled)
```
You are Telos, an AI programming assistant specialized in Haskell development.
...

## Available Tools

### bash
Execute bash commands...

### read
Read file contents...

## Environment
- Working directory: /mnt/data/.../Telos
- Git repository: yes
- Platform: linux
- Date: 2026-01-11

## Project Rules (from /path/to/AGENTS.md)

1. This is a Haskell project using GHC 9.12...
```

## Trade-offs

| Approach | Pros | Cons |
|----------|------|------|
| Embedded prompts | Simple, no IO | Hard to customize |
| External files | Flexible | Deployment complexity |
| Dynamic building | Context-aware | Slight startup overhead |

**Decision**: Use embedded base prompt + dynamic AGENTS.md discovery. Best balance of simplicity and flexibility.

## Open Questions

1. Should we cache AGENTS.md content or re-read on each conversation?
   - Recommendation: Re-read per conversation (files may change)

2. Should we support `.telosrc` in addition to `AGENTS.md`?
   - Defer: Start with AGENTS.md for opencode compatibility

3. Maximum prompt size limits?
   - Monitor token usage, add truncation if needed

## Implementation Results

### Phase 1: Core Infrastructure ✅
- `Telos.Prompt.Types`: `PromptSection`, `SystemPromptConfig` with lenses
- `Telos.Prompt.Builder`: `buildSystemPrompt`, section generators

### Phase 2: AGENTS.md Discovery ✅
- `Telos.Prompt.Discovery`: 
  - `discoverAgentsRules` - main entry point
  - `findAgentsFiles` - recursive upward search from working dir
  - `getGlobalAgentsFile` - checks `~/.config/telos/AGENTS.md`
  - Returns `[PromptSection]` with priorities 100, 110, 120... (global last)

### Phase 3: Integration ✅
- `AgentConfig.acPromptConfig :: Maybe SystemPromptConfig`
- `buildSystemPrompt` now returns `IO Text` (filesystem access)
- `runAgentLoop` calls `buildSystemPrompt` dynamically each iteration

### Deviations from Original Design
- None significant
