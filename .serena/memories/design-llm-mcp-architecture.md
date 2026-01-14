# Design Log: LLM Chat + MCP 调用架构

## 设计日期: 2026-01-10
## 状态: 草案待确认

---

## 背景

Telos 是一个 Haskell agentic 编程工具。本设计覆盖 LLM 聊天和 MCP 工具调用的核心模块。

### 技术栈选择
- **效果系统**: Polysemy
- **流处理**: Conduit
- **MCP 协议**: 复用 `Tritlo/mcp` 类型 + 自定义 Client 实现
- **LLM 后端**: 可扩展多 Provider 架构

---

## 问题

### 关键发现
1. **`Tritlo/mcp` 库只提供 Server 实现**，无 Client
2. 需要自己实现 MCP Client：进程管理 + JSON-RPC 通信
3. Polysemy 与 Conduit 集成需要 `transPipe` 或 `embed` 模式

### 需要解决的问题
1. 如何设计可扩展的 LLM Provider 效果？
2. MCP Client 如何管理多个 Server 进程？
3. 流式响应如何在 Polysemy 效果栈中传递？
4. Agent Loop 如何编排 LLM 调用和工具执行？

---

## 设计

### 模块结构

```
src/Telos/
├── Core/
│   ├── Types.hs              -- 核心类型 (Message, ToolCall, etc.)
│   ├── Error.hs              -- 错误类型
│   └── Config.hs             -- 配置类型
│
├── Effect/
│   ├── LLM.hs                -- LLM 效果定义
│   ├── MCP.hs                -- MCP 效果定义
│   ├── Logger.hs             -- 日志效果
│   └── Process.hs            -- 进程管理效果
│
├── LLM/
│   ├── Provider.hs           -- Provider 运行器
│   ├── OpenAI.hs             -- OpenAI 解释器
│   ├── Claude.hs             -- Claude 解释器
│   └── Streaming.hs          -- 流式处理工具
│
├── MCP/
│   ├── Client.hs             -- MCP Client 核心
│   ├── Types.hs              -- MCP 类型 (可复用 mcp 库)
│   ├── JsonRpc.hs            -- JSON-RPC 编解码
│   ├── Transport/
│   │   └── StdIO.hs          -- StdIO 传输实现
│   └── ServerManager.hs      -- 多 Server 生命周期管理
│
├── Agent/
│   ├── Loop.hs               -- Agent 主循环
│   ├── Context.hs            -- 对话上下文
│   └── Orchestrator.hs       -- 工具执行编排
│
└── App.hs                    -- 应用入口 + 效果栈组合
```

---

### 核心类型定义

```haskell
-- src/Telos/Core/Types.hs

-- | 消息角色
data Role = User | Assistant | System | Tool
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 聊天消息
data Message
  = UserMessage      { content :: Text }
  | AssistantMessage { content :: Maybe Text, toolCalls :: [ToolCall] }
  | SystemMessage    { content :: Text }
  | ToolResultMessage
      { toolCallId :: Text
      , toolName   :: Text
      , result     :: Text
      , isError    :: Bool
      }
  deriving (Eq, Show, Generic)

-- | 工具调用
data ToolCall = ToolCall
  { tcId        :: Text
  , tcName      :: Text
  , tcArguments :: Value  -- JSON object
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 工具定义
data Tool = Tool
  { toolName        :: Text
  , toolDescription :: Maybe Text
  , toolInputSchema :: Value  -- JSON Schema
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 流式事件
data StreamEvent
  = ContentDelta Text           -- 增量文本
  | ToolCallStart ToolCall      -- 工具调用开始
  | ToolCallDelta Text Text     -- (toolCallId, argumentsDelta)
  | StreamEnd                   -- 流结束
  | StreamError Text            -- 错误
  deriving (Eq, Show)
```

---

### 效果定义

#### LLM 效果

```haskell
-- src/Telos/Effect/LLM.hs
{-# LANGUAGE GADTs, TemplateHaskell #-}

module Telos.Effect.LLM where

import Polysemy
import Conduit

-- | LLM 效果
data LLM m a where
  -- | 非流式聊天
  Chat :: [Message] -> [Tool] -> LLM m (Either LLMError AssistantMessage)
  
  -- | 流式聊天，返回 Conduit 源
  ChatStream 
    :: [Message] 
    -> [Tool] 
    -> LLM m (ConduitT () StreamEvent IO (), IO (Either LLMError AssistantMessage))
  
  -- | 获取当前 Provider 信息
  GetProviderInfo :: LLM m ProviderInfo

makeSem ''LLM

-- | Provider 信息
data ProviderInfo = ProviderInfo
  { providerName    :: Text
  , providerModel   :: Text
  , supportsTools   :: Bool
  , supportsVision  :: Bool
  }
```

#### MCP 效果

```haskell
-- src/Telos/Effect/MCP.hs
{-# LANGUAGE GADTs, TemplateHaskell #-}

module Telos.Effect.MCP where

import Polysemy

-- | MCP 效果
data MCP m a where
  -- | 列出所有可用工具
  ListTools :: MCP m [Tool]
  
  -- | 调用工具
  CallTool :: Text -> Value -> MCP m (Either MCPError ToolResult)
  
  -- | 列出资源
  ListResources :: MCP m [Resource]
  
  -- | 读取资源
  ReadResource :: Text -> MCP m (Either MCPError ResourceContent)
  
  -- | 获取服务器状态
  GetServerStatus :: Text -> MCP m (Maybe ServerStatus)

makeSem ''MCP

-- | 工具执行结果
data ToolResult = ToolResult
  { trContent :: [ContentItem]
  , trIsError :: Bool
  }

data ContentItem
  = TextContent Text
  | ImageContent Text Text  -- (mimeType, base64Data)
  | ResourceContent Text Text  -- (uri, text)
```

#### 进程效果

```haskell
-- src/Telos/Effect/Process.hs
{-# LANGUAGE GADTs, TemplateHaskell #-}

module Telos.Effect.Process where

import Polysemy
import System.Process (ProcessHandle)

-- | 进程管理效果
data ProcessEff m a where
  SpawnProcess 
    :: FilePath           -- command
    -> [String]           -- args  
    -> Maybe FilePath     -- working directory
    -> ProcessEff m (Either ProcessError ProcessHandle)
  
  SendToProcess :: ProcessHandle -> ByteString -> ProcessEff m ()
  
  ReadFromProcess :: ProcessHandle -> ProcessEff m ByteString
  
  TerminateProcess :: ProcessHandle -> ProcessEff m ()
  
  IsProcessRunning :: ProcessHandle -> ProcessEff m Bool

makeSem ''ProcessEff
```

---

### MCP Client 实现

```haskell
-- src/Telos/MCP/Client.hs

module Telos.MCP.Client where

import Polysemy
import qualified Data.Aeson as A
import Telos.MCP.JsonRpc
import Telos.Effect.Process

-- | MCP Server 连接
data MCPConnection = MCPConnection
  { connServerName :: Text
  , connProcess    :: ProcessHandle
  , connStdin      :: Handle
  , connStdout     :: Handle
  , connRequestId  :: TVar Int
  , connCapabilities :: ServerCapabilities
  }

-- | 初始化 MCP 连接
initializeMCPServer
  :: Members '[ProcessEff, Embed IO] r
  => ServerConfig
  -> Sem r (Either MCPError MCPConnection)
initializeMCPServer config = do
  -- 1. 启动进程
  result <- spawnProcess (scCommand config) (scArgs config) (scWorkDir config)
  case result of
    Left err -> pure $ Left (ProcessSpawnError err)
    Right (ph, stdin, stdout, _stderr) -> do
      requestIdVar <- embed $ newTVarIO 1
      
      -- 2. 发送 initialize 请求
      let initReq = InitializeRequest
            { irProtocolVersion = "2024-11-05"
            , irCapabilities = clientCapabilities
            , irClientInfo = ClientInfo "Telos" "0.1.0"
            }
      
      response <- sendRequest stdin stdout requestIdVar "initialize" initReq
      
      case response of
        Left err -> pure $ Left err
        Right initResp -> do
          -- 3. 发送 initialized 通知
          sendNotification stdin "notifications/initialized" ()
          
          pure $ Right MCPConnection
            { connServerName = scName config
            , connProcess = ph
            , connStdin = stdin
            , connStdout = stdout
            , connRequestId = requestIdVar
            , connCapabilities = irCapabilities initResp
            }

-- | 调用工具
callToolImpl
  :: Members '[Embed IO] r
  => MCPConnection
  -> Text
  -> Value
  -> Sem r (Either MCPError ToolResult)
callToolImpl conn toolName arguments = do
  let req = CallToolRequest
        { ctrName = toolName
        , ctrArguments = arguments
        }
  sendRequest (connStdin conn) (connStdout conn) (connRequestId conn) 
              "tools/call" req
```

---

### LLM Provider 解释器

```haskell
-- src/Telos/LLM/OpenAI.hs

module Telos.LLM.OpenAI where

import Polysemy
import Conduit
import Network.HTTP.Client
import qualified Data.Aeson as A

-- | OpenAI 配置
data OpenAIConfig = OpenAIConfig
  { oaiApiKey   :: Text
  , oaiModel    :: Text
  , oaiBaseUrl  :: Text
  , oaiManager  :: Manager
  }

-- | 运行 LLM 效果为 OpenAI
runLLMOpenAI
  :: Members '[Embed IO] r
  => OpenAIConfig
  -> Sem (LLM ': r) a
  -> Sem r a
runLLMOpenAI config = interpret $ \case
  
  Chat messages tools -> do
    let request = buildChatRequest config messages tools False
    embed $ do
      response <- httpLbs request (oaiManager config)
      pure $ parseNonStreamResponse (responseBody response)
  
  ChatStream messages tools -> do
    let request = buildChatRequest config messages tools True
    embed $ do
      -- 返回 Conduit 源 + 最终结果的 IO action
      resultVar <- newEmptyMVar
      let source = httpSource request (oaiManager config) $= parseSSEConduit resultVar
      let getFinal = takeMVar resultVar
      pure (source, getFinal)
  
  GetProviderInfo -> pure ProviderInfo
    { providerName = "OpenAI"
    , providerModel = oaiModel config
    , supportsTools = True
    , supportsVision = oaiModel config `elem` ["gpt-4o", "gpt-4-vision-preview"]
    }

-- | SSE 解析 Conduit
parseSSEConduit 
  :: MVar (Either LLMError AssistantMessage)
  -> ConduitT ByteString StreamEvent IO ()
parseSSEConduit resultVar = do
  stateRef <- liftIO $ newIORef initialParseState
  awaitForever $ \chunk -> do
    events <- liftIO $ parseSSEChunk stateRef chunk
    forM_ events $ \event ->
      case event of
        SSEDone finalMsg -> do
          liftIO $ putMVar resultVar (Right finalMsg)
          yield StreamEnd
        SSEDelta delta -> yield (ContentDelta delta)
        SSEToolCall tc -> yield (ToolCallStart tc)
        SSEError err -> do
          liftIO $ putMVar resultVar (Left err)
          yield (StreamError err)
```

---

### Agent 主循环

```haskell
-- src/Telos/Agent/Loop.hs

module Telos.Agent.Loop where

import Polysemy
import Conduit

-- | Agent 循环效果
data AgentLoop m a where
  RunConversation :: Text -> AgentLoop m ConversationResult

makeSem ''AgentLoop

-- | 运行 Agent 循环
runAgentLoop
  :: Members '[LLM, MCP, Logger, Embed IO] r
  => AgentConfig
  -> Sem (AgentLoop ': r) a
  -> Sem r a
runAgentLoop config = interpret $ \case
  RunConversation userInput -> do
    -- 初始化上下文
    context <- initContext config
    
    -- 添加用户消息
    let messages = contextMessages context ++ [UserMessage userInput]
    
    -- 获取可用工具
    tools <- listTools
    
    -- Agent 循环
    runLoop messages tools
  where
    runLoop messages tools = do
      -- 1. 调用 LLM
      result <- chat messages tools
      
      case result of
        Left err -> do
          log Error $ "LLM error: " <> show err
          pure $ ConversationError err
        
        Right assistantMsg -> do
          let newMessages = messages ++ [toMessage assistantMsg]
          
          -- 2. 检查是否有工具调用
          if null (toolCalls assistantMsg)
            then do
              -- 无工具调用，对话结束
              pure $ ConversationComplete assistantMsg
            else do
              -- 3. 执行工具调用
              toolResults <- forM (toolCalls assistantMsg) $ \tc -> do
                log Info $ "Calling tool: " <> tcName tc
                result <- callTool (tcName tc) (tcArguments tc)
                pure $ ToolResultMessage
                  { toolCallId = tcId tc
                  , toolName = tcName tc
                  , result = formatToolResult result
                  , isError = either (const True) trIsError result
                  }
              
              -- 4. 添加工具结果并继续循环
              let withResults = newMessages ++ map toMessage toolResults
              runLoop withResults tools
```

---

### 应用效果栈

```haskell
-- src/Telos/App.hs

module Telos.App where

import Polysemy
import Polysemy.Error
import Polysemy.State

-- | 应用效果栈
type AppEffects =
  '[ AgentLoop
   , LLM
   , MCP
   , ProcessEff
   , Logger
   , Error AppError
   , State AppState
   , Embed IO
   , Final IO
   ]

-- | 运行应用
runApp :: AppConfig -> Sem AppEffects a -> IO (Either AppError a)
runApp config =
    runFinal
  . embedToFinal @IO
  . runStateIORef (appStateRef config)
  . runError
  . runLoggerToStdout
  . runProcessIO
  . runMCPWithServers (mcpServers config)
  . runLLMWithProvider (llmProvider config)
  . runAgentLoop (agentConfig config)
```

---

## 实现计划

### Phase 1: 基础设施 (2-3天)
1. [ ] 项目设置 (package.yaml, 扩展, 依赖)
2. [ ] 核心类型定义 (`Core/Types.hs`, `Core/Error.hs`)
3. [ ] 效果定义 (`Effect/*.hs`)
4. [ ] 日志效果实现

### Phase 2: MCP Client (3-4天)
1. [ ] JSON-RPC 编解码 (`MCP/JsonRpc.hs`)
2. [ ] StdIO 传输实现
3. [ ] MCP Client 核心 (`initializeMCPServer`, `callTool`)
4. [ ] Server Manager (多服务器管理)
5. [ ] MCP 效果解释器

### Phase 3: LLM Provider (3-4天)
1. [ ] OpenAI 解释器 (含流式)
2. [ ] Claude 解释器
3. [ ] SSE 解析 Conduit
4. [ ] Provider 配置加载

### Phase 4: Agent Loop (2-3天)
1. [ ] 对话上下文管理
2. [ ] Agent 循环实现
3. [ ] 工具执行编排
4. [ ] 错误恢复策略

### Phase 5: 集成测试 (2天)
1. [ ] 单元测试 (各效果)
2. [ ] 集成测试 (端到端)
3. [ ] 示例应用

---

## 待决问题

### Q1: 是否复用 `mcp` 库的类型？
**选项 A**: 完全复用 `MCP.Types`  
**选项 B**: 仅复用基础类型，Client 部分自定义  
**选项 C**: 完全自定义  
**建议**: B - 保持灵活性

### Q2: 流式响应如何暴露给上层？
**当前设计**: 返回 `(ConduitT () StreamEvent IO (), IO FinalResult)`  
**替代方案**: 纯 Conduit，最终结果通过 Conduit 最后一个元素传递  
**需要确认**: 用户偏好？

### Q3: 多 Provider 切换策略？
**选项 A**: 编译时选择 (不同解释器)  
**选项 B**: 运行时选择 (配置驱动)  
**选项 C**: 混合 (默认 + fallback)

---

## 参考资料

- MCP 规范: https://spec.modelcontextprotocol.io/
- Tritlo/mcp 库: https://github.com/Tritlo/mcp
- Polysemy 文档: https://hackage.haskell.org/package/polysemy
- OpenAI API: https://platform.openai.com/docs/api-reference
