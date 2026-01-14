# Design #010: Dynamic Context Pruning (DCP)

## Background

As conversations grow longer, context windows fill up with:
- Outdated tool outputs (file contents that were later modified)
- Duplicate information (same file read multiple times)
- Failed tool outputs that are no longer relevant
- Large outputs that have been summarized or acted upon

OpenCode's DCP plugin solves this by:
1. Automatically marking stale content for pruning
2. Providing `discard` and `extract` tools for LLM-driven pruning
3. Replacing pruned content with placeholders (not deleting from history)

## Problem

Telos currently has no context management. Long sessions will:
- Hit token limits
- Slow down responses
- Include irrelevant/outdated information

## Questions & Answers

**Q1: Should pruning modify session history?**
A1: No. Like OpenCode, we transform messages before sending to LLM, but keep original history intact. This allows undo/redo and session replay.

**Q2: Where does pruning happen in the pipeline?**
A2: In `chat.messages.transform` hook - after loading history, before sending to LLM.

**Q3: How do we identify prunable content?**
A3: Each tool result gets a unique ID. We track which IDs are marked for pruning in session state.

**Q4: What strategies should we implement?**
A4: Start with core three:
- Deduplication: Same tool+params called multiple times → keep latest
- Supersede Writes: Write followed by Read of same file → prune write
- Purge Errors: Failed tool calls older than N turns → prune inputs

---

## Design

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent Loop                              │
├─────────────────────────────────────────────────────────────┤
│  loadHistory() → transformMessages() → sendToLLM()          │
│                        ↓                                     │
│              ┌─────────────────────┐                        │
│              │  Pruning Pipeline   │                        │
│              ├─────────────────────┤                        │
│              │ 1. syncToolCache    │ ← Track tool IDs       │
│              │ 2. deduplicate      │ ← Mark duplicates      │
│              │ 3. supersedeWrites  │ ← Mark stale writes    │
│              │ 4. purgeErrors      │ ← Mark old errors      │
│              │ 5. applyPruning     │ ← Replace with placeholders │
│              │ 6. injectPruneList  │ ← Add <prunable-tools> │
│              └─────────────────────┘                        │
└─────────────────────────────────────────────────────────────┘
```

### New Types

```haskell
-- src/Telos/Context/Types.hs

-- | Unique identifier for a tool result in conversation
newtype ToolResultId = ToolResultId { unToolResultId :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | Reason for pruning
data PruneReason
  = PruneDuplicate      -- ^ Duplicate tool call, newer exists
  | PruneSuperseded     -- ^ Write superseded by subsequent read
  | PruneError          -- ^ Error output older than threshold
  | PruneManualDiscard  -- ^ LLM explicitly discarded
  | PruneManualExtract  -- ^ LLM extracted and discarded
  deriving stock (Eq, Show, Generic)

-- | Information about a prunable tool result
data PrunableInfo = PrunableInfo
  { piId         :: ToolResultId
  , piToolName   :: Text
  , piParams     :: Text        -- ^ Summary of params (e.g., file path)
  , piTokens     :: Int         -- ^ Estimated tokens
  , piTurnAge    :: Int         -- ^ How many turns ago
  } deriving stock (Eq, Show, Generic)

-- | Pruning state per session
data PruneState = PruneState
  { psMarkedIds    :: Set ToolResultId      -- ^ IDs marked for pruning
  , psToolCache    :: Map ToolResultId ToolCacheEntry  -- ^ Metadata cache
  , psDistillations :: Map ToolResultId Text -- ^ Extracted summaries
  , psStats        :: PruneStats
  } deriving stock (Eq, Show, Generic)

data ToolCacheEntry = ToolCacheEntry
  { tceToolName  :: Text
  , tceParams    :: Value       -- ^ Original parameters
  , tceParamKey  :: Text        -- ^ Normalized key for dedup (tool:param_hash)
  , tceTurnIndex :: Int
  , tceIsError   :: Bool
  , tceFilePath  :: Maybe FilePath  -- ^ For file operations
  } deriving stock (Eq, Show, Generic)

data PruneStats = PruneStats
  { psTotalPruned    :: Int
  , psTokensSaved    :: Int
  , psByStrategy     :: Map Text Int  -- ^ Count per strategy
  } deriving stock (Eq, Show, Generic)
```

### Pruning Strategies

```haskell
-- src/Telos/Context/Strategy.hs

-- | Strategy that marks tool results for pruning
type PruneStrategy = PruneState -> [Message] -> PruneState

-- | Deduplication: keep only most recent of duplicate tool calls
deduplicationStrategy :: Set Text -> PruneStrategy  -- protected tools
deduplicationStrategy protected state msgs = ...
  -- Group by (toolName, paramHash)
  -- For each group, mark all but most recent

-- | Supersede writes: prune writes when file was subsequently read
supersedeWritesStrategy :: PruneStrategy
supersedeWritesStrategy state msgs = ...
  -- Track write/edit operations by filePath
  -- If same file was read later, mark write for pruning

-- | Purge errors: prune error outputs older than N turns
purgeErrorsStrategy :: Int -> PruneStrategy  -- turn threshold
purgeErrorsStrategy threshold state msgs = ...
  -- Find error tool results older than threshold
  -- Mark their inputs for pruning (keep error message)
```

### Discard/Extract Tools

```haskell
-- src/Telos/Tool/Discard.hs

discardTool :: BuiltinTool
-- Input: { "ids": ["noise", "5", "12"] }  -- first element is reason
-- Validates IDs against prunable list
-- Marks IDs for pruning in PruneState
-- Returns confirmation with tokens saved

-- src/Telos/Tool/Extract.hs

extractTool :: BuiltinTool
-- Input: { "ids": ["5", "12"], "distillation": ["summary1", "summary2"] }
-- Stores distillations in PruneState
-- Marks IDs for pruning
-- Returns confirmation
```

### Message Transformation

```haskell
-- src/Telos/Context/Transform.hs

-- | Transform messages before sending to LLM
transformForLLM :: PruneState -> [Message] -> [Message]
transformForLLM state = map (transformMessage state)

transformMessage :: PruneState -> Message -> Message
transformMessage state msg = case msg of
  ToolResultMessage id content
    | id `Set.member` psMarkedIds state ->
        case Map.lookup id (psDistillations state) of
          Just distill -> msg { content = distill }
          Nothing -> msg { content = "[Output removed to save context]" }
  _ -> msg

-- | Inject prunable tools list for LLM awareness
injectPrunableList :: PruneState -> [Message] -> [Message]
injectPrunableList state msgs = msgs ++ [systemReminder]
  where
    prunable = getPrunableTools state msgs
    systemReminder = SystemMessage $ formatPrunableList prunable
```

### Integration Points

```haskell
-- In Agent/Loop.hs

agentStep :: ... -> Sem r ()
agentStep ctx = do
  -- Load history
  history <- getHistory ctx
  
  -- Run pruning pipeline
  pruneState <- getPruneState ctx
  let pruneState' = runStrategies pruneState history
  setPruneState ctx pruneState'
  
  -- Transform for LLM
  let transformed = transformForLLM pruneState' history
      withPrunableList = injectPrunableList pruneState' transformed
  
  -- Send to LLM
  response <- chat model withPrunableList
  ...
```

### Configuration

```haskell
-- In CLI/Config.hs

data PruneConfig = PruneConfig
  { pcEnabled         :: Bool
  , pcTurnProtection  :: Int      -- ^ Don't prune last N turns
  , pcProtectedTools  :: [Text]   -- ^ Never auto-prune these
  , pcErrorThreshold  :: Int      -- ^ Turns before error pruning
  , pcNudgeFrequency  :: Int      -- ^ Remind LLM every N turns
  } deriving stock (Eq, Show, Generic)

defaultPruneConfig :: PruneConfig
defaultPruneConfig = PruneConfig
  { pcEnabled = True
  , pcTurnProtection = 2
  , pcProtectedTools = ["task"]  -- Don't prune subagent results
  , pcErrorThreshold = 3
  , pcNudgeFrequency = 5
  }
```

---

## Implementation Plan

### Phase 1: Core Types & State (1 hour)
- [ ] Create `src/Telos/Context/Types.hs` with all types
- [ ] Create `src/Telos/Context/State.hs` for state management
- [ ] Add `PruneState` to `AgentContext`
- [ ] Add `PruneConfig` to `CliConfig`

### Phase 2: Tool ID Tracking (1 hour)
- [ ] Modify `Message` type to include optional `ToolResultId`
- [ ] Generate unique IDs in `executeToolCalls`
- [ ] Build tool cache during message scan

### Phase 3: Strategies (1.5 hours)
- [ ] Implement `deduplicationStrategy`
- [ ] Implement `supersedeWritesStrategy`
- [ ] Implement `purgeErrorsStrategy`
- [ ] Create strategy pipeline runner

### Phase 4: Discard/Extract Tools (1 hour)
- [ ] Implement `discardTool`
- [ ] Implement `extractTool`
- [ ] Add to tool registry

### Phase 5: Message Transform (1 hour)
- [ ] Implement `transformForLLM`
- [ ] Implement `injectPrunableList`
- [ ] Integrate into agent loop

### Phase 6: Testing (1 hour)
- [ ] Unit tests for strategies
- [ ] Integration test for full pipeline
- [ ] Manual testing with real conversations

---

## Examples

### Deduplication

```
Turn 1: read("/src/main.hs") → [id=1] "module Main..."
Turn 3: read("/src/main.hs") → [id=5] "module Main..." (updated)

After deduplication:
- id=1 marked for pruning (duplicate, older)
- id=5 kept (most recent)
```

### Supersede Writes

```
Turn 1: edit("/src/main.hs", old, new) → [id=1] "Replaced 1 occurrence..."
Turn 2: read("/src/main.hs") → [id=2] "module Main... (with changes)"

After supersede-writes:
- id=1 marked for pruning (write superseded by read)
- id=2 kept (has current content)
```

### LLM-Driven Pruning

```
<prunable-tools>
1: read, /src/old-file.hs
5: bash, npm install
12: grep, "TODO" in src/
</prunable-tools>

LLM: I'll clean up context that's no longer needed.
[Uses discard with ids: ["completion", "1", "5"]]
```

---

## Trade-offs

| Pro | Con |
|-----|-----|
| Extends effective context window | Additional complexity in message flow |
| LLM can manage its own context | Token cost for prunable list injection |
| Automatic stale content removal | Risk of pruning still-relevant content |
| Preserves original history | Memory overhead for state tracking |

---

**Status**: Draft  
**Dependencies**: None  
**Estimated effort**: 6-7 hours
