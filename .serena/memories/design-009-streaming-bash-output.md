# Design #009: Streaming Bash Tool Output

## Background

Currently, the bash tool uses `readProcess` which blocks until the command completes, then returns all output at once. For long-running commands (e.g., `stack build`, `npm install`), users see no output until the command finishes.

OpenCode and similar tools stream command output in real-time, providing better UX.

## Problem

1. `ToolExecutor = ToolContext -> Value -> IO ToolResult` - pure IO, no streaming callback
2. `executeBash` uses `readProcess` (blocking)
3. `executeBuiltinTool` runs in `IO`, not in `Sem r` with `StreamOutput` effect
4. No mechanism to push incremental output during tool execution

## Questions and Answers

**Q1: Should we stream to stdout directly or use the StreamOutput effect?**
A1: Both. Stream to stdout for immediate display, but also need to capture full output for ToolResult.

**Q2: Should streaming be opt-in per tool or automatic for bash?**
A2: Automatic for bash. Other tools (read, write, glob) return structured data that doesn't benefit from streaming.

**Q3: How to handle stderr vs stdout?**
A3: Interleave them as they arrive (like a real terminal). Prefix stderr lines with color/marker if needed.

**Q4: What about timeout handling with streaming?**
A4: Keep existing timeout logic, but apply to the entire streaming process.

## Design

### Option A: Callback-based Streaming Executor (Chosen)

Add a new executor type that takes an output callback:

```haskell
-- In Tool/Types.hs
type StreamCallback = Text -> IO ()

data ToolExecutorType
  = SimpleExecutor ToolExecutor
  | StreamingExecutor (StreamCallback -> ToolContext -> Value -> IO ToolResult)
  | AgentExecutor
```

The callback is called for each chunk of output. The final `ToolResult` contains the complete output.

### Option B: Conduit-based

Use `ConduitT` for streaming, but this adds complexity and doesn't integrate well with existing `IO`-based executor pattern.

### Implementation

#### Phase 1: Add StreamingExecutor type

```haskell
-- src/Telos/Tool/Types.hs
type StreamCallback = Text -> IO ()

data ToolExecutorType
  = SimpleExecutor ToolExecutor
  | StreamingExecutor (StreamCallback -> ToolContext -> Value -> IO ToolResult)
  | AgentExecutor
```

#### Phase 2: Rewrite executeBash with streaming

```haskell
-- src/Telos/Tool/Bash.hs
executeBashStreaming :: StreamCallback -> ToolContext -> Value -> IO ToolResult
executeBashStreaming onChunk ctx args = do
  -- Parse args (same as before)
  -- Create process with pipes
  let cp = (proc "bash" ["-c", command])
           { std_out = CreatePipe
           , std_err = CreatePipe
           , cwd = workdir
           }
  
  withCreateProcess cp $ \_ mOut mErr ph -> do
    -- Spawn threads to read stdout/stderr
    outputVar <- newTVarIO ""
    
    let readHandle h = do
          eof <- hIsEOF h
          unless eof $ do
            line <- hGetLine h
            let chunk = line <> "\n"
            onChunk chunk  -- Stream to user
            atomically $ modifyTVar' outputVar (<> chunk)
            readHandle h
    
    -- Read both handles concurrently
    withAsync (readHandle stdout) $ \_ ->
      withAsync (readHandle stderr) $ \_ ->
        waitForProcess ph
    
    output <- readTVarIO outputVar
    -- Return ToolResult with full output
```

#### Phase 3: Update executeBuiltinTool

```haskell
-- src/Telos/Agent/Loop.hs
executeBuiltinTool :: (Text -> IO ()) -> ToolContext -> Text -> Value -> IO (Maybe ToolResult)
executeBuiltinTool onChunk ctx name args = do
  case lookupTool name of
    Nothing -> pure Nothing
    Just tool -> case tool ^. btExecutor of
      SimpleExecutor exec -> Just <$> exec ctx args
      StreamingExecutor exec -> Just <$> exec onChunk ctx args
      AgentExecutor -> pure Nothing  -- Handled separately
```

#### Phase 4: Wire up in agent loop

```haskell
-- In executeToolCalls
mBuiltinResult <- embed $ executeBuiltinTool outputCallback toolCtx tName toolArgs
  where
    outputCallback chunk = do
      TIO.putStr chunk
      hFlush stdout
```

### File Changes

| File | Changes |
|------|---------|
| `src/Telos/Tool/Types.hs` | Add `StreamCallback`, `StreamingExecutor` |
| `src/Telos/Tool/Bash.hs` | Rewrite with streaming, use `createProcess` |
| `src/Telos/Tool/Registry.hs` | Update `executeBuiltinTool` signature |
| `src/Telos/Agent/Loop.hs` | Pass output callback to `executeBuiltinTool` |

### Example Output

Before:
```
[Tool: bash]
(nothing for 30 seconds...)
<entire output dumps at once>
```

After:
```
[Tool: bash]
Compiling Module1...
Compiling Module2...
(real-time output as it happens)
```

## Trade-offs

| Pro | Con |
|-----|-----|
| Real-time feedback for long commands | Slightly more complex executor type |
| Better UX, matches user expectations | Need to handle concurrent stdout/stderr |
| Output still captured in ToolResult | Minor performance overhead from callbacks |

## Migration

- Existing `SimpleExecutor` tools unchanged
- Only bash tool converted to `StreamingExecutor`
- Backward compatible

---

## Implementation Results

**Status**: ✅ Completed

### Files Changed

| File | Changes |
|------|---------|
| `src/Telos/Tool/Types.hs` | Added `StreamCallback`, `StreamingExecutor`, updated `ToolExecutorType` |
| `src/Telos/Tool/Bash.hs` | Rewrote with `createProcess` + pipes, async readers, callback-based streaming |
| `src/Telos/Tool/Registry.hs` | Added `isStreamingTool`, updated `executeBuiltinTool` signature |
| `src/Telos/Agent/Loop.hs` | Wired up `streamCallback` that outputs to stdout in real-time |
| `test/Telos/Tool/BashSpec.hs` | Updated `runExecutor` helper to support `StreamingExecutor` |

### Key Implementation Details

1. **Concurrent Handle Reading**: Uses `Control.Concurrent.Async` to read stdout/stderr simultaneously
2. **Line-by-Line Streaming**: Reads each line and immediately calls the callback
3. **Output Accumulation**: Full output still captured in `ToolResult` for history
4. **Callback in Loop.hs**: Simply `TIO.putStr chunk >> hFlush stdout`

### Test Results

```
193 examples, 0 failures
```

### Deviations from Design

1. **No `System.IO` imports needed** - Relude provides `hFlush`, `hIsEOF`, `hSetBuffering`
2. **Simplified callback** - Direct stdout output instead of going through `StreamOutput` effect (simpler, works immediately)

---
**Status**: Implemented  
**Dependencies**: None (uses base async/process)  
**Actual effort**: ~30 minutes
