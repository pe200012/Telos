# Design Log #003: Streaming Response Support

## Background

Phase 4 实现了基于非流式 `chat` 的 Agent Loop。现在需要支持流式响应，实现：
1. 使用 `ChatStream` 替代 `Chat`
2. 实时显示 LLM 响应
3. 流式中断处理 (Ctrl+C)

## Current State

### 已有类型 (Core/Types.hs)
```haskell
data StreamEvent 
  = ContentDelta Text           -- 文本增量
  | ToolCallStart { ... }       -- 工具调用开始 (id, name)
  | ToolCallDelta { ... }       -- 工具调用参数增量
  | Ping

data StreamResult
  = StreamCompleted AssistantMessage  -- 完整消息
  | StreamInterrupted PartialMessage  -- 被中断的部分消息
  | StreamFailed Text                 -- 失败

data PartialMessage = PartialMessage
  { pmContentSoFar :: Text
  , pmToolCallsSoFar :: [PartialToolCall]
  }
```

### 已有 Effect (Effect/LLM.hs)
```haskell
data LLM m a where
  Chat :: [Message] -> [Tool] -> LLM m (Either LLMError AssistantMessage)
  ChatStream :: [Message] -> [Tool] -> LLM m (ConduitT () StreamEvent IO StreamResult)
```

### 问题

1. **Interpreter 不完整**: `runLLMWithCopilot` 的 `ChatStream` 实现返回 `StreamFailed`，没有累积结果
2. **Loop 不支持流式**: `agentStep` 只用 `chat`，不处理 `chatStream`
3. **中断处理缺失**: 流式过程中如何检查中断并优雅停止

## Questions & Answers

**Q1: 流式累积应该在哪里实现？**
A1: 在 Conduit 消费端。Interpreter 只负责解析 SSE → StreamEvent，Loop 负责累积和中断检查。

**Q2: 中断检查的频率？**
A2: 每个 StreamEvent 都检查一次。ContentDelta 通常很小（几个字符），频率合适。

**Q3: 中断后如何处理部分消息？**
A3: 返回 `StreamInterrupted PartialMessage`，Loop 决定是否保存到历史。当前设计：中断后不保存部分消息。

**Q4: ToolCall 流式如何处理？**
A4: SSE 中 tool_calls 是增量的：
- 先收到 `ToolCallStart` (index, id, name)
- 然后多个 `ToolCallDelta` (index, arguments 片段)
- 需要按 index 累积 arguments

**Q5: 实时输出在哪里做？**
A5: 新增 `StreamOutput` effect，Loop 调用它输出 ContentDelta。Interpreter 可以是 stdout/回调。

## Design

### 架构图

```
┌─────────────────────────────────────────────────────────────────┐
│                         Agent Loop                               │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │ agentStepStreaming                                         │  │
│  │   1. chatStream messages tools → conduit                   │  │
│  │   2. runStreamWithInterrupt conduit →                      │  │
│  │        - 每个 event: 检查中断, 累积, 输出                    │  │
│  │   3. StreamResult → 继续 loop 或返回                        │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
         │                              │
         ▼                              ▼
┌─────────────────┐           ┌─────────────────┐
│  LLM Effect     │           │ StreamOutput    │
│  (ChatStream)   │           │ Effect          │
└─────────────────┘           └─────────────────┘
         │                              │
         ▼                              ▼
┌─────────────────┐           ┌─────────────────┐
│ Copilot SSE     │           │ stdout / TVar   │
│ → StreamEvent   │           │ / callback      │
└─────────────────┘           └─────────────────┘
```

### 新增模块

#### 1. `src/Telos/Effect/StreamOutput.hs`
```haskell
data StreamOutput m a where
  OutputChunk :: Text -> StreamOutput m ()      -- 输出文本增量
  OutputToolStart :: Text -> StreamOutput m ()  -- 输出 "Calling tool: xxx"
  FlushOutput :: StreamOutput m ()              -- 刷新缓冲区

outputChunk :: Member StreamOutput r => Text -> Sem r ()
outputToolStart :: Member StreamOutput r => Text -> Sem r ()
flushOutput :: Member StreamOutput r => Sem r ()
```

#### 2. `src/Telos/Effect/StreamOutput/IO.hs`
```haskell
-- 输出到 stdout，实时显示
runStreamOutputIO :: Member (Embed IO) r => Sem (StreamOutput ': r) a -> Sem r a
```

#### 3. `src/Telos/Agent/Streaming.hs` (核心)
```haskell
-- 消费流并累积结果，支持中断
runStreamWithInterrupt 
  :: (Member (Embed IO) r, Member StreamOutput r)
  => MVar ()                           -- interrupt signal
  -> ConduitT () StreamEvent IO StreamResult  -- source
  -> Sem r StreamResult

-- 累积 StreamEvent 到 PartialMessage
data StreamAccumulator = StreamAccumulator
  { saContent :: !Text
  , saToolCalls :: !(IntMap PartialToolCall)  -- index → partial
  }

emptyAccumulator :: StreamAccumulator
accumulate :: StreamAccumulator -> StreamEvent -> StreamAccumulator
finalizeAccumulator :: StreamAccumulator -> PartialMessage
```

#### 4. 修改 `src/Telos/Agent/Loop.hs`
```haskell
-- 新增流式版本
agentStepStreaming 
  :: (Member LLM r, Member MCP r, Member (Embed IO) r, Member StreamOutput r, Member Logger r)
  => AgentContext
  -> Sem r AgentResult

-- runAgentLoop 改为使用 agentStepStreaming
-- 或新增 runAgentLoopStreaming
```

### 流式处理流程

```
1. chatStream messages tools → ConduitT () StreamEvent IO StreamResult
2. Loop 开始消费 conduit:
   for each event:
     a. checkInterrupted → if True: return StreamInterrupted (finalize accumulator)
     b. case event of
          ContentDelta t → 
            - accumulate content
            - outputChunk t  (实时输出)
          ToolCallStart idx id name → 
            - init toolcall at index
            - outputToolStart name
          ToolCallDelta idx args →
            - append args to toolcall at index
          Ping → continue
3. Conduit 结束 → 获取 StreamResult
   - StreamCompleted msg → 返回给 loop
   - StreamFailed err → 返回 AgentError
```

### Copilot Interpreter 修改

当前 `chatResponseToStreamEvent` 基本正确，但需要修复：
1. 确保 `parseChunks` 正确处理 `[DONE]`
2. 返回值改为在 conduit 内部累积，最终 yield `StreamCompleted`

实际上，更好的设计是：
- Conduit 只 yield `StreamEvent`
- **最终结果通过 conduit 的返回值** (`StreamResult`) 返回
- Loop 负责消费 + 累积

当前签名 `ConduitT () StreamEvent IO StreamResult` 已经是这样设计的。

### 中断处理

```haskell
runStreamWithInterrupt :: MVar () -> ConduitT () StreamEvent IO StreamResult -> Sem r StreamResult
runStreamWithInterrupt interruptVar conduit = embed $ do
  acc <- newIORef emptyAccumulator
  result <- runConduit $ conduit .| processEvents interruptVar acc
  interrupted <- checkInterrupted interruptVar
  if interrupted
    then StreamInterrupted <$> (finalizeAccumulator <$> readIORef acc)
    else pure result

processEvents :: MVar () -> IORef StreamAccumulator -> ConduitT StreamEvent Void IO ()
processEvents interruptVar accRef = awaitForever $ \event -> do
  interrupted <- liftIO $ checkInterruptedPure interruptVar
  when interrupted $ do
    -- 停止消费，让上层处理
    pure ()
  unless interrupted $ do
    liftIO $ modifyIORef' accRef (`accumulate` event)
    case event of
      ContentDelta t -> liftIO $ T.putStr t >> hFlush stdout
      ToolCallStart _ _ name -> liftIO $ putStrLn $ "\n[Calling: " <> T.unpack name <> "]"
      _ -> pure ()
```

### 类型签名变化

```haskell
-- Before (Loop.hs)
agentStep :: (...) => AgentContext -> Sem r AgentResult

-- After: 两个版本
agentStep :: (...) => AgentContext -> Sem r AgentResult           -- 非流式
agentStepStreaming :: (..., Member StreamOutput r) => AgentContext -> Sem r AgentResult  -- 流式
```

### AgentConfig 扩展

```haskell
data AgentConfig = AgentConfig
  { acMaxIterations :: Int
  , acSystemPrompt :: Maybe Text
  , acMCPServers :: [MCPServerConfig]
  , acStreamingEnabled :: Bool   -- NEW: 是否启用流式
  }
```

## Implementation Plan

### Phase 1: StreamOutput Effect
- [ ] 创建 `src/Telos/Effect/StreamOutput.hs`
- [ ] 创建 `src/Telos/Effect/StreamOutput/IO.hs`
- [ ] 添加到 telos.cabal exposed-modules

### Phase 2: Streaming 累积逻辑
- [ ] 创建 `src/Telos/Agent/Streaming.hs`
- [ ] 实现 `StreamAccumulator`
- [ ] 实现 `runStreamWithInterrupt`

### Phase 3: LLM Interpreter 修复
- [ ] 修复 `runLLMWithCopilot` 的 `ChatStream` 分支
- [ ] 确保 conduit 正确返回 `StreamCompleted`

### Phase 4: Agent Loop 集成
- [ ] 添加 `agentStepStreaming` 到 Loop.hs
- [ ] 修改 `runAgentLoop` 支持流式模式
- [ ] 更新 `AgentConfig` 添加 `acStreamingEnabled`

### Phase 5: App 集成
- [ ] 更新 `App.hs` effect stack 添加 `StreamOutput`
- [ ] 更新 REPL 使用流式

### Phase 6: 测试
- [ ] `test/Telos/Agent/StreamingSpec.hs` - 累积逻辑测试
- [ ] `test/Telos/Effect/StreamOutputSpec.hs` - effect 测试
- [ ] 更新 LoopSpec 测试流式分支

## Examples

### ✅ 正常流式响应
```
User: What is 2+2?
Assistant: The answer is 4.  ← 实时逐字显示
```

### ✅ 流式工具调用
```
User: What's the weather?
[Calling: get_weather]       ← 检测到 tool call
{"location": "Beijing"}      ← 执行工具
Assistant: The weather in Beijing is sunny.
```

### ✅ 中断处理
```
User: Tell me a long story
Assistant: Once upon a ti^C  ← 用户按 Ctrl+C
[Interrupted]
> _                          ← 返回提示符
```

### ❌ 错误：中断后继续输出
```
User: Tell me a story
Assistant: Once upon a ti^C
me there was...              ← 不应该继续
```

## Trade-offs

| 决策 | 优点 | 缺点 |
|------|------|------|
| 每个 event 检查中断 | 响应快 (<100ms) | 轻微性能开销 |
| 不保存中断的部分消息 | 简单、干净 | 丢失部分内容 |
| 分离 StreamOutput effect | 可测试、可替换 | 多一个 effect |
| IntMap 累积 tool calls | O(log n) 查找 | 比 Vector 稍慢 |

## Open Questions

None currently.

---

## Implementation Results

### Completed: 2026-01-10

All phases implemented successfully:

1. **StreamOutput Effect** (`Effect/StreamOutput.hs`, `Effect/StreamOutput/IO.hs`)
   - `outputChunk`, `outputToolStart`, `outputToolEnd`, `outputNewline`, `flushOutput`
   - IO interpreter writes to stdout with immediate flush
   - Silent interpreter for testing

2. **Agent/Streaming.hs**
   - `StreamAccumulator` with `IntMap` for tool calls by index
   - `accumulate` handles ContentDelta, ToolCallStart, ToolCallDelta, Ping
   - `consumeStreamWithInterrupt` checks MVar before each event
   - `accumulatorToAssistantMessage` parses JSON arguments

3. **LLM/Interpreter.hs**
   - `ChatStream` returns conduit that yields `StreamEvent`
   - Consumer (Agent/Streaming) builds final message from accumulated events

4. **Agent/Loop.hs**
   - Added `runAgentLoopStreaming` and `agentStepStreaming`
   - Uses `consumeStreamWithInterrupt` with real-time output handler
   - Shared `handleAssistantMessage` for tool execution

5. **AgentConfig**
   - Added `acStreamingEnabled :: Bool` (default True)

6. **App.hs**
   - Effect stack includes `StreamOutput`
   - REPL uses streaming or non-streaming based on config

### Test Results
- 71 tests passing (21 new streaming tests)
- `StreamingSpec`: 17 tests for accumulator logic
- `StreamOutputSpec`: 4 tests for effect

### Deviations from Design
- `streamEventHandler` in Loop.hs uses direct IO instead of StreamOutput effect for simplicity
- Conduit returns placeholder `StreamResult`; actual result built by consumer from accumulated events
