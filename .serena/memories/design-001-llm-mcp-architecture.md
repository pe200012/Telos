# Design Log #001: LLM Chat + MCP 调用架构

## 设计日期: 2026-01-10
## 状态: ✅ 已确认

---

## 背景

Telos 是一个 Haskell agentic 编程工具。本设计覆盖 LLM 聊天和 MCP 工具调用的核心模块。

### 技术栈
- **效果系统**: Polysemy
- **流处理**: Conduit (带 StreamResult 返回值)
- **MCP 协议**: 自定义类型 (复制 Tritlo/mcp)
- **LLM 后端**: GitHub Copilot (原生 OAuth) + 可扩展架构

### 关键设计决策
| 问题 | 决策 |
|------|------|
| MCP 类型 | 自定义，复制 `Tritlo/mcp` 的 Types.hs |
| 流式返回 | `ConduitT () StreamEvent IO StreamResult` |
| 中断支持 | ✅ StreamInterrupted 返回部分结果 |
| Provider | Copilot 原生 OAuth，可扩展多后端 |

---

## 模块结构

```
src/Telos/
├── Core/
│   ├── Types.hs              -- Message, ToolCall, Tool, StreamEvent, StreamResult
│   └── Error.hs              -- LLMError, MCPError, AppError
│
├── Effect/
│   ├── LLM.hs                -- LLM 效果 (Chat, ChatStream)
│   ├── MCP.hs                -- MCP 效果 (ListTools, CallTool)
│   ├── Process.hs            -- 进程管理效果
│   └── Logger.hs             -- 日志效果
│
├── LLM/
│   ├── Types.hs              -- Provider 相关类型
│   ├── Copilot/
│   │   ├── Auth.hs           -- OAuth Device Flow
│   │   ├── Client.hs         -- API 客户端
│   │   └── Interpreter.hs    -- runLLMCopilot
│   ├── OpenAI/               -- (未来扩展)
│   │   └── Interpreter.hs
│   └── Streaming.hs          -- SSE 解析, runConduitWithInterrupt
│
├── MCP/
│   ├── Types.hs              -- MCP 协议类型
│   ├── JsonRpc.hs            -- JSON-RPC 2.0 编解码
│   ├── Client.hs             -- MCP Client 核心
│   ├── Transport/
│   │   └── StdIO.hs          -- stdin/stdout 传输
│   └── ServerManager.hs      -- 多服务器生命周期
│
├── Agent/
│   ├── Loop.hs               -- Agent 主循环
│   ├── Context.hs            -- 对话上下文
│   └── Interrupt.hs          -- 中断处理
│
└── App.hs                    -- 应用入口 + 效果栈
```

---

## 核心类型定义

### 消息类型 (`Core/Types.hs`)

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telos.Core.Types where

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 消息角色
data Role = User | Assistant | System | Tool
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 聊天消息
data Message
  = UserMessage 
      { umContent :: Text 
      }
  | AssistantMessage 
      { amContent   :: Maybe Text
      , amToolCalls :: [ToolCall] 
      }
  | SystemMessage 
      { smContent :: Text 
      }
  | ToolResultMessage
      { trmToolCallId :: Text
      , trmToolName   :: Text
      , trmResult     :: Text
      , trmIsError    :: Bool
      }
  deriving (Eq, Show, Generic)

-- | 工具调用
data ToolCall = ToolCall
  { tcId        :: Text
  , tcName      :: Text
  , tcArguments :: Value  -- JSON object
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 工具定义
data Tool = Tool
  { toolName        :: Text
  , toolDescription :: Maybe Text
  , toolInputSchema :: Value  -- JSON Schema
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

### 流式类型

```haskell
-- | 流式事件
data StreamEvent
  = ContentDelta Text              -- 增量文本
  | ToolCallStart                  -- 工具调用开始
      { tcsIndex :: Int
      , tcsId    :: Text
      , tcsName  :: Text
      }
  | ToolCallDelta                  -- 工具调用参数增量
      { tcdIndex     :: Int
      , tcdArguments :: Text
      }
  | Ping                           -- 保活
  deriving (Eq, Show)

-- | 流式结果 (Conduit 返回值)
data StreamResult
  = StreamCompleted AssistantMessage  -- 正常完成
  | StreamInterrupted PartialMessage  -- 用户中断
  | StreamFailed LLMError             -- 错误
  deriving (Eq, Show)

-- | 部分消息 (中断时)
data PartialMessage = PartialMessage
  { pmContentSoFar   :: Text
  , pmToolCallsSoFar :: [PartialToolCall]
  }
  deriving (Eq, Show)

data PartialToolCall = PartialToolCall
  { ptcId            :: Maybe Text
  , ptcName          :: Maybe Text
  , ptcArgumentsSoFar :: Text
  }
  deriving (Eq, Show)
```

---

## 效果定义

### LLM 效果 (`Effect/LLM.hs`)

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Telos.Effect.LLM where

import Polysemy
import Data.Conduit (ConduitT)
import Telos.Core.Types

-- | LLM 效果
data LLM m a where
  -- | 非流式聊天 (简化场景)
  Chat 
    :: [Message] 
    -> [Tool] 
    -> LLM m (Either LLMError AssistantMessage)
  
  -- | 流式聊天 (主要使用)
  ChatStream 
    :: [Message] 
    -> [Tool] 
    -> LLM m (ConduitT () StreamEvent IO StreamResult)
  
  -- | 获取 Provider 信息
  GetProviderInfo 
    :: LLM m ProviderInfo

makeSem ''LLM

-- | Provider 信息
data ProviderInfo = ProviderInfo
  { piName         :: Text
  , piModel        :: Text
  , piSupportsTools :: Bool
  , piMaxTokens    :: Maybe Int
  }
  deriving (Eq, Show)
```

### MCP 效果 (`Effect/MCP.hs`)

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.Effect.MCP where

import Polysemy
import Data.Aeson (Value)
import Telos.Core.Types
import Telos.MCP.Types

-- | MCP 效果
data MCP m a where
  -- | 列出所有可用工具 (聚合所有 MCP servers)
  ListTools :: MCP m [Tool]
  
  -- | 调用工具
  CallTool :: Text -> Value -> MCP m (Either MCPError ToolResult)
  
  -- | 列出资源
  ListResources :: MCP m [Resource]
  
  -- | 读取资源
  ReadResource :: Text -> MCP m (Either MCPError ResourceContent)

makeSem ''MCP

-- | 工具执行结果
data ToolResult = ToolResult
  { trContent :: [ContentItem]
  , trIsError :: Bool
  }
  deriving (Eq, Show)

data ContentItem
  = TextContent Text
  | ImageContent Text Text  -- mimeType, base64Data
  | ResourceContent Text Text  -- uri, text
  deriving (Eq, Show)
```

---

## GitHub Copilot 实现

### OAuth Device Flow (`LLM/Copilot/Auth.hs`)

```haskell
module Telos.LLM.Copilot.Auth where

import Data.Text (Text)
import Network.HTTP.Client

-- | Copilot OAuth 配置
copilotClientId :: Text
copilotClientId = "Iv1.b507a08c87ecfe98"

-- | Token 状态
data TokenState
  = NoToken
  | PendingAuth DeviceCodeResponse
  | Authenticated CopilotToken
  | TokenExpired CopilotToken  -- 需要刷新
  deriving (Show)

data DeviceCodeResponse = DeviceCodeResponse
  { dcrDeviceCode      :: Text
  , dcrUserCode        :: Text
  , dcrVerificationUri :: Text
  , dcrExpiresIn       :: Int
  , dcrInterval        :: Int
  }
  deriving (Show, Generic, FromJSON)

data CopilotToken = CopilotToken
  { ctToken     :: Text       -- ghu_xxx
  , ctExpiresAt :: UTCTime
  , ctEndpoints :: Endpoints
  }
  deriving (Show)

-- | 启动 Device Flow
initiateDeviceFlow :: Manager -> IO (Either AuthError DeviceCodeResponse)

-- | 轮询等待用户授权
pollForToken :: Manager -> DeviceCodeResponse -> IO (Either AuthError CopilotToken)

-- | 刷新 Copilot API token
refreshCopilotToken :: Manager -> Text -> IO (Either AuthError CopilotToken)
```

### API 客户端 (`LLM/Copilot/Client.hs`)

```haskell
module Telos.LLM.Copilot.Client where

-- | Copilot 配置
data CopilotConfig = CopilotConfig
  { ccToken   :: TVar TokenState
  , ccManager :: Manager
  , ccModel   :: Text  -- 默认 "gpt-4.1"
  }

-- | 创建聊天请求
createChatRequest 
  :: CopilotConfig 
  -> [Message] 
  -> [Tool] 
  -> Bool       -- stream
  -> IO Request

-- | 必需的请求头
copilotHeaders :: CopilotToken -> [Header]
copilotHeaders token =
  [ ("Authorization", "Bearer " <> encodeUtf8 (ctToken token))
  , ("copilot-integration-id", "vscode-chat")
  , ("editor-version", "vscode/1.95.0")
  , ("editor-plugin-version", "copilot-chat/0.26.7")
  , ("user-agent", "GitHubCopilotChat/0.26.7")
  , ("x-github-api-version", "2025-04-01")
  , ("X-Initiator", "user")  -- or "agent" if has tool results
  ]
```

### 解释器 (`LLM/Copilot/Interpreter.hs`)

```haskell
module Telos.LLM.Copilot.Interpreter where

import Polysemy
import Data.Conduit
import Telos.Effect.LLM
import Telos.LLM.Streaming

-- | 运行 LLM 效果 (Copilot 实现)
runLLMCopilot
  :: Members '[Embed IO] r
  => CopilotConfig
  -> Sem (LLM ': r) a
  -> Sem r a
runLLMCopilot config = interpret $ \case
  
  Chat messages tools -> embed $ do
    ensureValidToken config
    request <- createChatRequest config messages tools False
    response <- httpLbs request (ccManager config)
    parseNonStreamResponse (responseBody response)
  
  ChatStream messages tools -> embed $ do
    ensureValidToken config
    request <- createChatRequest config messages tools True
    pure $ httpSourceWithInterrupt request (ccManager config)
          .| parseSSEConduit
          .| collectStreamResult
  
  GetProviderInfo -> pure ProviderInfo
    { piName = "GitHub Copilot"
    , piModel = ccModel config
    , piSupportsTools = True
    , piMaxTokens = Just 16384
    }
```

---

## 流式处理与中断 (`LLM/Streaming.hs`)

```haskell
module Telos.LLM.Streaming where

import Data.Conduit
import Control.Concurrent.STM

-- | 支持中断的 HTTP 源
httpSourceWithInterrupt
  :: Request
  -> Manager
  -> ConduitT () ByteString IO ()

-- | SSE 解析 Conduit
parseSSEConduit :: ConduitT ByteString StreamEvent IO ()

-- | 收集流事件并构建最终结果
collectStreamResult :: ConduitT StreamEvent StreamEvent IO StreamResult
collectStreamResult = do
  stateRef <- liftIO $ newIORef initialCollectorState
  
  let loop = do
        mEvent <- await
        case mEvent of
          Nothing -> do
            -- 流正常结束
            state <- liftIO $ readIORef stateRef
            pure $ StreamCompleted (buildAssistantMessage state)
          
          Just event -> do
            liftIO $ updateCollectorState stateRef event
            yield event  -- 传递给下游 (UI 显示)
            loop
  
  loop

-- | 带中断检查的运行器
runConduitWithInterrupt
  :: TVar Bool                                    -- 中断信号
  -> ConduitT () StreamEvent IO StreamResult      -- 源 conduit
  -> (StreamEvent -> IO ())                       -- 事件处理器
  -> IO StreamResult
runConduitWithInterrupt interruptVar source handler = do
  collectorRef <- newIORef initialCollectorState
  
  let sink = awaitForever $ \event -> do
        -- 检查中断
        interrupted <- liftIO $ readTVarIO interruptVar
        when interrupted $ do
          state <- liftIO $ readIORef collectorRef
          -- 提前终止并返回部分结果
          leftover event  -- 可选：保留未处理事件
          error "interrupted"  -- 用异常控制流
        
        -- 正常处理
        liftIO $ do
          handler event
          updateCollectorState collectorRef event
  
  result <- try $ runConduit $ source .| sink
  
  case result of
    Right finalResult -> pure finalResult
    Left (SomeException _) -> do
      state <- readIORef collectorRef
      pure $ StreamInterrupted (buildPartialMessage state)
```

---

## MCP Client 实现

### JSON-RPC (`MCP/JsonRpc.hs`)

```haskell
module Telos.MCP.JsonRpc where

import Data.Aeson

-- | JSON-RPC 请求
data JsonRpcRequest = JsonRpcRequest
  { jrqJsonrpc :: Text      -- "2.0"
  , jrqId      :: RequestId
  , jrqMethod  :: Text
  , jrqParams  :: Maybe Value
  }
  deriving (Generic, ToJSON)

-- | JSON-RPC 响应
data JsonRpcResponse = JsonRpcResponse
  { jrsJsonrpc :: Text
  , jrsId      :: RequestId
  , jrsResult  :: Maybe Value
  , jrsError   :: Maybe JsonRpcError
  }
  deriving (Generic, FromJSON)

data JsonRpcError = JsonRpcError
  { jreCode    :: Int
  , jreMessage :: Text
  , jreData    :: Maybe Value
  }
  deriving (Generic, FromJSON)

newtype RequestId = RequestId (Either Int Text)
  deriving (Eq, Show, ToJSON, FromJSON)
```

### MCP Client (`MCP/Client.hs`)

```haskell
module Telos.MCP.Client where

import System.Process
import System.IO
import Control.Concurrent.STM

-- | MCP 服务器连接
data MCPConnection = MCPConnection
  { mcName       :: Text
  , mcProcess    :: ProcessHandle
  , mcStdin      :: Handle
  , mcStdout     :: Handle
  , mcNextId     :: TVar Int
  , mcCapabilities :: ServerCapabilities
  }

-- | 启动并初始化 MCP 服务器
spawnMCPServer
  :: ServerConfig
  -> IO (Either MCPError MCPConnection)
spawnMCPServer config = do
  -- 1. 创建进程
  let cp = (proc (scCommand config) (scArgs config))
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe  -- 用于日志
        , cwd = scWorkDir config
        }
  
  (Just stdin, Just stdout, Just stderr, ph) <- createProcess cp
  
  -- 设置 handle 模式
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  
  nextIdVar <- newTVarIO 1
  
  -- 2. 发送 initialize
  let initParams = InitializeParams
        { ipProtocolVersion = "2024-11-05"
        , ipCapabilities = ClientCapabilities { ... }
        , ipClientInfo = ClientInfo "Telos" "0.1.0"
        }
  
  response <- sendRequest stdin stdout nextIdVar "initialize" initParams
  
  case response of
    Left err -> do
      terminateProcess ph
      pure $ Left err
    Right initResult -> do
      -- 3. 发送 initialized 通知
      sendNotification stdin "notifications/initialized" ()
      
      pure $ Right MCPConnection
        { mcName = scName config
        , mcProcess = ph
        , mcStdin = stdin
        , mcStdout = stdout
        , mcNextId = nextIdVar
        , mcCapabilities = irCapabilities initResult
        }

-- | 调用工具
callToolOnServer
  :: MCPConnection
  -> Text
  -> Value
  -> IO (Either MCPError ToolResult)
callToolOnServer conn toolName arguments = do
  let params = CallToolParams
        { ctpName = toolName
        , ctpArguments = arguments
        }
  sendRequest (mcStdin conn) (mcStdout conn) (mcNextId conn) "tools/call" params
```

---

## Agent Loop (`Agent/Loop.hs`)

```haskell
module Telos.Agent.Loop where

import Polysemy
import Polysemy.State
import Control.Concurrent.STM

-- | Agent 状态
data AgentState = AgentState
  { asMessages    :: [Message]
  , asInterrupted :: TVar Bool
  }

-- | 运行 Agent 对话
runAgent
  :: Members '[LLM, MCP, Logger, Embed IO] r
  => AgentConfig
  -> Text           -- 用户输入
  -> Sem r AgentResult
runAgent config userInput = do
  -- 获取工具
  tools <- listTools
  
  -- 初始化消息
  let messages = systemPrompt config : [UserMessage userInput]
  
  -- 运行 agent loop
  loop messages tools
  where
    loop messages tools = do
      -- 获取流式 conduit
      source <- chatStream messages tools
      
      -- 创建中断信号
      interruptVar <- embed $ newTVarIO False
      
      -- 运行流并显示
      result <- embed $ runConduitWithInterrupt interruptVar source displayToTerminal
      
      case result of
        StreamFailed err -> do
          logError $ "LLM error: " <> show err
          pure $ AgentError err
        
        StreamInterrupted partial -> do
          logInfo "User interrupted generation"
          -- 保存部分结果
          let partialMsg = partialToAssistant partial
          pure $ AgentInterrupted (messages ++ [partialMsg])
        
        StreamCompleted assistantMsg -> do
          let newMessages = messages ++ [toMessage assistantMsg]
          
          if null (amToolCalls assistantMsg)
            then do
              -- 无工具调用，对话完成
              pure $ AgentComplete assistantMsg newMessages
            else do
              -- 执行工具调用
              toolResults <- forM (amToolCalls assistantMsg) $ \tc -> do
                logInfo $ "Calling tool: " <> tcName tc
                result <- callTool (tcName tc) (tcArguments tc)
                pure $ ToolResultMessage
                  { trmToolCallId = tcId tc
                  , trmToolName = tcName tc
                  , trmResult = formatResult result
                  , trmIsError = either (const True) trIsError result
                  }
              
              -- 继续循环
              loop (newMessages ++ toolResults) tools

data AgentResult
  = AgentComplete AssistantMessage [Message]
  | AgentInterrupted [Message]
  | AgentError LLMError
```

---

## 实现计划

### Phase 1: 基础设施 (2天)
- [ ] 项目设置 (package.yaml, cabal, 扩展)
- [ ] Core/Types.hs - 所有核心类型
- [ ] Core/Error.hs - 错误类型
- [ ] Effect/*.hs - 效果定义 (不含解释器)

### Phase 2: MCP Client (3天)
- [ ] MCP/Types.hs - 复制并调整 Tritlo/mcp 类型
- [ ] MCP/JsonRpc.hs - JSON-RPC 编解码
- [ ] MCP/Transport/StdIO.hs - 进程通信
- [ ] MCP/Client.hs - 连接管理
- [ ] MCP/ServerManager.hs - 多服务器
- [ ] Effect/MCP.hs 解释器

### Phase 3: LLM Provider (3天)
- [ ] LLM/Copilot/Auth.hs - OAuth Device Flow
- [ ] LLM/Copilot/Client.hs - API 客户端
- [ ] LLM/Streaming.hs - SSE 解析 + 中断
- [ ] LLM/Copilot/Interpreter.hs - runLLMCopilot

### Phase 4: Agent Loop (2天)
- [ ] Agent/Context.hs - 上下文管理
- [ ] Agent/Loop.hs - 主循环
- [ ] Agent/Interrupt.hs - 中断处理
- [ ] App.hs - 效果栈组合

### Phase 5: 测试与集成 (2天)
- [ ] 单元测试 (JSON-RPC, SSE 解析)
- [ ] 集成测试 (MCP, LLM)
- [ ] CLI 示例

---

## 依赖列表

```yaml
dependencies:
  - base >= 4.15 && < 5
  - text
  - bytestring
  - aeson >= 2.0
  - containers
  - unordered-containers
  - vector
  - mtl
  - transformers
  - polysemy >= 1.9
  - polysemy-plugin
  - conduit >= 1.3
  - conduit-extra
  - http-client >= 0.7
  - http-client-tls
  - http-types
  - http-conduit
  - process >= 1.6
  - async >= 2.2
  - stm >= 2.5
  - time
  - filepath
  - directory
  - unliftio
```

---

## 参考资料

- MCP 规范: https://spec.modelcontextprotocol.io/
- GitHub Copilot API: 见 `github-copilot-api-research.md`
- Polysemy: https://hackage.haskell.org/package/polysemy
- Conduit: https://hackage.haskell.org/package/conduit
