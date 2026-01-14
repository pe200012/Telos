# LLM Chat + MCP 调用 Haskell 实现研究

## 研究日期: 2026-01-10

---

## 1. 现有 MCP Haskell 实现

### 推荐: Tritlo/mcp ⭐⭐⭐⭐⭐
- **Hackage**: `mcp` (v0.3.0.0)
- **GitHub**: https://github.com/Tritlo/mcp
- **协议版本**: MCP 2025-06-18 (最新)
- **许可**: MIT
- **特性**:
  - 完整 MCP 协议 (resources, tools, prompts, sampling, elicitation)
  - 双传输层: StdIO + HTTP (带 OAuth 2.0)
  - 类型安全设计
  - Server-Sent Events (SSE) 支持
- **模块结构**:
  ```
  MCP.Types         -- 核心类型
  MCP.Protocol      -- JSON-RPC 包装
  MCP.Server        -- MCPServer 类型类
  MCP.Server.StdIO  -- StdIO 传输
  MCP.Server.HTTP   -- HTTP 传输
  ```

### 备选: drshade/haskell-mcp-server
- **Hackage**: `mcp-server` (v0.1.0.15)
- **特性**: Template Haskell 自动派生 handler

### 备选: buecking/hs-mcp
- 轻量级, 协议版本 2024-11-05

---

## 2. JSON-RPC 库

### GaloisInc/argo (企业级)
- 用于 Cryptol 和 SAW
- 支持 StdIO, HTTP, Socket 传输
- 显式状态管理

### haskell/lsp
- Language Server Protocol 实现
- 与 MCP 架构相似
- 消息类型: RequestMessage, NotificationMessage, ResponseMessage

### Hackage 上其他库
- `json-rpc` (1.1.1) - 完整 JSON-RPC 2.0
- `servant-jsonrpc` (1.2.0) - Servant 集成
- `jsonrpc-tinyclient` (1.0.1.0) - 极简客户端

---

## 3. LLM API 客户端

### OpenAI: MercuryTechnologies/openai ⭐⭐⭐⭐⭐
- **Hackage**: `openai` (>=2.2)
- **特性**:
  - 完整流式响应 (SSE) 支持
  - Servant 类型安全 API
  - Chat Completions, Embeddings, Audio, Images
- **流式示例**:
  ```haskell
  let onEvent (Right Chat.ChatCompletionChunk{choices}) =
        mapM_ (\ChunkChoice{delta} -> 
          maybe (pure ()) TIO.putStr (delta_content delta)
        ) choices
  createChatCompletionStreamTyped req onEvent
  ```

### Claude: T0mLam/claude-haskell ⭐⭐⭐
- Git dependency (未发布到 Hackage)
- 支持图片、PDF 输入
- ⚠️ 不支持流式响应

### 统一抽象: tusharad/langchain-hs
- 跨 LLM 统一接口 (OpenAI, Gemini, DeepSeek)

---

## 4. 关键技术模式

### JSON-RPC 2.0 消息格式
```haskell
-- 请求
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text        -- "2.0"
  , id :: RequestId        -- Text | Int | Null
  , method :: Text
  , params :: Maybe Value
  }

-- 响应
data JsonRpcResponse = JsonRpcResponse
  { jsonrpc :: Text
  , id :: RequestId
  , result :: Maybe Value
  , error :: Maybe JsonRpcError
  }

-- 通知 (无 id, 无响应)
data JsonRpcNotification = JsonRpcNotification
  { jsonrpc :: Text
  , method :: Text
  , params :: Maybe Value
  }
```

### MCP 协议关键流程
1. **初始化**: 
   - Client → Server: `initialize` {protocolVersion, capabilities, clientInfo}
   - Server → Client: response {protocolVersion, capabilities, serverInfo}
   - Client → Server: `notifications/initialized`

2. **工具调用**:
   - `tools/list` → 返回工具定义列表 (支持分页)
   - `tools/call` {name, arguments} → 返回 {content, isError}

### StdIO 传输
- 通过 stdin/stdout 通信
- 消息以换行符分隔
- 消息内不能包含换行符
- stderr 用于日志

### 进程管理 (Haskell)
```haskell
import System.Process

withCreateProcess cp{std_in=CreatePipe, std_out=CreatePipe} $
  \(Just stdin) (Just stdout) _ ph -> do
    hPutStrLn stdin (encode request)
    response <- hGetLine stdout
    -- ...
```

---

## 5. 设计决策点

### Q1: 使用现有库还是自己实现?
- **选项 A**: 使用 `Tritlo/mcp` (推荐，最完整)
- **选项 B**: 使用 `GaloisInc/argo` 构建
- **选项 C**: 从零实现 (更多控制)

### Q2: LLM 客户端选择?
- **选项 A**: `openai` 库 (最成熟，支持流式)
- **选项 B**: 多后端抽象层 (支持 OpenAI + Claude + ...)
- **选项 C**: 自定义 HTTP 客户端

### Q3: 流式处理方式?
- **选项 A**: Callback 模式 (openai 库风格)
- **选项 B**: Conduit 流处理
- **选项 C**: Streaming 库

### Q4: 并发模型?
- **选项 A**: async + STM
- **选项 B**: effectful 效果系统
- **选项 C**: 简单 IO + MVar

---

## 6. 推荐依赖

```yaml
dependencies:
  - base >= 4.15
  - openai >= 2.2           # OpenAI API
  - mcp                     # MCP 协议 (Tritlo)
  - aeson                   # JSON
  - text
  - bytestring
  - http-client-tls         # HTTP
  - process                 # 进程管理
  - async                   # 并发
  - stm                     # 共享状态
```
