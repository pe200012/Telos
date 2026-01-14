# Design #006: Agent Orchestration (Subagents)

## Background

Telos currently supports a single agent loop that processes user input, calls tools, and returns results. For complex tasks, we need the ability to **delegate subtasks** to child agents (subagents) that run with isolated conversation history but share resources like tools and MCP connections.

This is essential for:
- Breaking complex tasks into manageable subtasks
- Parallel execution of independent work
- Specialized agent behaviors (e.g., code review, exploration)

## Problem

The parent agent needs to:
1. Spawn a subagent with a specific task prompt
2. Wait for the subagent to complete (synchronous) or continue working (async)
3. Receive the subagent's final response as a tool result
4. Share tools/MCP connections without re-initialization

## Questions and Answers

**Q1: Should subagents share conversation history with the parent?**
A1: No. Subagents get isolated history. They receive only the task prompt as initial context. This prevents context pollution and keeps subagent focus narrow.

**Q2: Should subagents share tools with the parent?**
A2: Yes. Tools (both builtin and MCP) should be shared. Re-initializing MCP connections is expensive and error-prone.

**Q3: How should interruption propagate?**
A3: Parent interrupt should propagate to all active subagents. Subagent errors should be captured and returned as tool results, not crash the parent.

**Q4: Sync vs Async execution?**
A4: Start with synchronous execution (simpler). Async can be added later as a separate `background_task` tool.

**Q5: Should subagents be able to spawn their own subagents?**
A5: Yes, with a configurable depth limit to prevent runaway recursion.

## Design

### New Builtin Tool: `task`

```haskell
-- Tool schema
taskTool :: Tool
taskTool = Tool
  { toolName = "task"
  , toolDescription = "Delegate a subtask to a new agent. The agent runs with isolated conversation history but shares tools."
  , toolInputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "prompt" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The task description for the subagent" :: Text)
              ]
          , "max_iterations" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Maximum iterations for the subagent (default: 10)" :: Text)
              ]
          ]
      , "required" .= (["prompt"] :: [Text])
      ]
  }
```

### SubagentConfig

```haskell
data SubagentConfig = SubagentConfig
  { sacPrompt        :: Text           -- Task prompt
  , sacMaxIterations :: Int            -- Iteration limit (default 10)
  , sacMaxDepth      :: Int            -- Max nesting depth (default 3)
  , sacCurrentDepth  :: Int            -- Current depth (0 for root)
  } deriving (Eq, Show)
```

### Context Isolation Strategy

```
┌─────────────────────────────────────────────────────┐
│                   Parent Agent                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │
│  │ History     │  │ Tools       │  │ Config      │  │
│  │ (isolated)  │  │ (SHARED)    │  │ (copied)    │  │
│  └─────────────┘  └──────┬──────┘  └─────────────┘  │
│                          │                           │
│         ┌────────────────┼────────────────┐         │
│         ▼                ▼                ▼         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │
│  │ Subagent A  │  │ Subagent B  │  │ Subagent C  │  │
│  │ (own hist)  │  │ (own hist)  │  │ (own hist)  │  │
│  └─────────────┘  └─────────────┘  └─────────────┘  │
└─────────────────────────────────────────────────────┘

Shared:     ctxTools, ctxToolContext (MCP connections)
Isolated:   ctxHistory, ctxIteration
Copied:     ctxConfig (with modified maxIterations)
Propagated: ctxInterrupt (parent interrupt → child interrupt)
```

### New Module: `Telos.Agent.Subagent`

```haskell
module Telos.Agent.Subagent
  ( runSubagent
  , createSubagentContext
  , SubagentConfig(..)
  , SubagentResult(..)
  ) where

import Telos.Agent.Context
import Telos.Agent.Loop
import Telos.Core.Types

data SubagentResult
  = SubagentSuccess Text      -- Final response text
  | SubagentError Text        -- Error message
  | SubagentMaxIterations     -- Hit iteration limit
  | SubagentInterrupted       -- Parent interrupted
  deriving (Eq, Show)

-- | Create isolated context for subagent
createSubagentContext 
  :: AgentContext    -- Parent context
  -> SubagentConfig  -- Subagent configuration
  -> IO AgentContext
createSubagentContext parent cfg = do
  -- Fresh history with just system prompt
  newHistory <- newTVarIO []
  
  -- Fresh iteration counter
  newIteration <- newTVarIO 0
  
  -- Copy config with adjusted max iterations
  parentConfig <- readTVarIO (ctxConfig parent)
  let childConfig = parentConfig 
        { acMaxIterations = sacMaxIterations cfg }
  newConfigVar <- newTVarIO childConfig
  
  pure AgentContext
    { ctxHistory     = newHistory        -- ISOLATED
    , ctxTools       = ctxTools parent   -- SHARED (same TVar)
    , ctxInterrupt   = ctxInterrupt parent -- SHARED (propagates)
    , ctxIteration   = newIteration      -- ISOLATED
    , ctxConfig      = newConfigVar      -- COPIED
    , ctxToolContext = ctxToolContext parent -- SHARED
    }

-- | Run a subagent and return its result
runSubagent
  :: Members '[LLM, MCP, Logger, StreamOutput, Embed IO] r
  => AgentContext
  -> SubagentConfig
  -> Sem r SubagentResult
runSubagent parentCtx cfg = do
  -- Check depth limit
  when (sacCurrentDepth cfg >= sacMaxDepth cfg) $
    pure $ SubagentError "Maximum subagent depth exceeded"
  
  -- Create child context
  childCtx <- embed $ createSubagentContext parentCtx cfg
  
  -- Run the agent loop with the task prompt
  result <- runAgentLoop childCtx (sacPrompt cfg)
  
  -- Convert AgentResult to SubagentResult
  pure $ case result of
    AgentResponse msg -> SubagentSuccess (amContent msg)
    AgentError err    -> SubagentError (show err)
    AgentMaxIterations -> SubagentMaxIterations
    AgentInterrupted  -> SubagentInterrupted
```

### Tool Executor Integration

Add to `Tool/Registry.hs`:

```haskell
-- In builtinTools map
, ("task", BuiltinTool taskTool executeTaskTool)

-- Executor (needs special handling - access to AgentContext)
executeTaskTool :: ToolContext -> Value -> IO ToolResult
executeTaskTool ctx args = do
  -- Parse arguments
  let prompt = args ^? key "prompt" . _String
      maxIter = fromMaybe 10 $ args ^? key "max_iterations" . _Integer
  
  case prompt of
    Nothing -> pure $ ToolResult False "Missing required parameter: prompt"
    Just p  -> do
      -- Need access to parent AgentContext - this requires refactoring
      -- See Implementation Plan Phase 2
      ...
```

### Challenge: BuiltinTool Executor Signature

Current signature:
```haskell
type ToolExecutor = ToolContext -> Value -> IO ToolResult
```

Problem: `task` tool needs access to `AgentContext` and Polysemy effects, not just `ToolContext`.

**Solution**: Introduce a new executor type for "agent-aware" tools:

```haskell
data BuiltinTool r = BuiltinTool
  { btTool     :: Tool
  , btExecutor :: ToolExecutorType r
  }

data ToolExecutorType r
  = SimpleExecutor (ToolContext -> Value -> IO ToolResult)
  | AgentExecutor  (AgentContext -> Value -> Sem r SubagentResult)
```

## Implementation Plan

### Phase 1: Core Types and Module Structure
1. Create `src/Telos/Agent/Subagent.hs` with `SubagentConfig`, `SubagentResult`
2. Implement `createSubagentContext`
3. Add `task` tool definition to Registry

### Phase 2: Executor Refactoring
1. Refactor `BuiltinTool` to support both simple and agent-aware executors
2. Update `executeBuiltinTool` to handle the new executor type
3. Thread `AgentContext` through tool execution path

### Phase 3: Integration
1. Implement `runSubagent` using `runAgentLoop`
2. Wire up `task` tool executor
3. Add depth tracking to prevent infinite recursion

### Phase 4: Testing
1. Unit tests for `createSubagentContext` isolation
2. Integration test: parent spawns subagent, receives result
3. Test interrupt propagation
4. Test depth limit enforcement

## Examples

### Good Usage ✅
```json
{
  "name": "task",
  "arguments": {
    "prompt": "Read the file src/Main.hs and summarize its structure",
    "max_iterations": 5
  }
}
```

### Bad Usage ❌
```json
{
  "name": "task",
  "arguments": {
    "prompt": ""  // Empty prompt
  }
}
```

## Trade-offs

| Decision | Pros | Cons |
|----------|------|------|
| Shared tools | No re-init, consistent state | Concurrent access concerns |
| Isolated history | Clean context, focused agent | Can't reference parent context |
| Sync-first | Simpler implementation | Blocks parent during subtask |
| Depth limit | Prevents runaway recursion | Limits complex delegation |

## Future Enhancements

1. **Async execution**: `background_task` tool that returns immediately with a task ID
2. **Subagent types**: Pre-configured agent personalities (explorer, reviewer, etc.)
3. **Context injection**: Optional ability to pass specific context to subagent
4. **Progress streaming**: Stream subagent progress back to parent

---

**Status**: Draft - Awaiting Review
