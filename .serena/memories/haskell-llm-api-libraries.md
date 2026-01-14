# Haskell LLM API 交互库调研报告

## 1. OpenAI API 客户端

### 1.1 openai (MercuryTechnologies)

**仓库**: https://github.com/MercuryTechnologies/openai
**Hackage**: https://hackage.haskell.org/package/openai
**状态**: ✅ 活跃维护,功能完整

#### 特性
- 使用 Servant 构建的类型安全 API 绑定
- 完整支持 OpenAI API v1
- **支持流式响应 (SSE)**
- 支持 Chat Completions, Audio, Embeddings, Fine-tuning, Files, Images 等

#### 核心依赖
```haskell
base, aeson, bytestring, containers
http-client, http-client-tls, http-types
servant, servant-client, servant-multipart
text, time, vector
```

#### 流式实现方式

使用 **http-client** 直接处理 SSE:

```haskell
-- 来自 OpenAI.V1
createChatCompletionStreamTyped
    :: CreateChatCompletion
    -> (Either Text Chat.Completions.ChatCompletionStreamEvent -> IO ())
    -> IO ()

-- 内部实现使用 HTTP.Client.withResponse + SSE 解析
-- 见 OpenAI.V1 lines 359-474
```

**实现细节** (OpenAI/V1.hs:359-474):
- 使用 `HTTP.Client.withResponse` 获取响应流
- 手动解析 SSE 格式 (`data: ` 前缀, `\n\n` 分隔符)
- 处理 `[DONE]` 终止信号
- 错误处理包含 HTTP 状态码检查

**示例代码**:
```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified OpenAI.V1 as V1
import qualified OpenAI.V1.Chat.Completions as Chat
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    key <- getEnv "OPENAI_KEY"
    env <- V1.getClientEnv "https://api.openai.com"
    
    let V1.Methods{createChatCompletionStreamTyped} = 
            V1.makeMethods env (pack key) Nothing Nothing
    
    let onEvent (Left err) = hPutStrLn stderr ("Error: " <> unpack err)
        onEvent (Right Chat.ChatCompletionChunk{Chat.choices = cs}) =
            mapM_ printChoice cs
          where
            printChoice Chat.ChunkChoice{Chat.delta = d} =
                case Chat.delta_content d of
                    Just content -> TIO.putStr content >> hFlush stdout
                    Nothing -> pure ()
    
    let req = Chat._CreateChatCompletion
            { Chat.messages = [Chat.User{content = [Chat.Text{text = "Hello!"}], name = Nothing}]
            , Chat.model = "gpt-4o"
            }
    
    createChatCompletionStreamTyped req onEvent
```

---

### 1.2 openai-servant-gen

**Hackage**: https://hackage.haskell.org/package/openai-servant-gen
**状态**: ⚠️ 自动生成,维护状态不明

#### 特性
- 基于 OpenAPI 规范自动生成
- Servant API + Client
- 依赖较多,可能更新不及时

---

### 1.3 openai-hs (社区实现)

**使用示例** (来自 mark-watson/haskell_tutorial_cookbook_examples):
```haskell
-- 来自 GenText.hs
import qualified OpenAI  -- 可能是不同的 openai-hs 包

completionRequestToString :: String -> IO String
completionRequestToString prompt = do
    -- 基础 HTTP 客户端封装
```

**状态**: ⚠️ 文档不完整,不推荐用于生产

---

## 2. Anthropic Claude API 客户端

### 2.1 claude-haskell (T0mLam)

**仓库**: https://github.com/T0mLam/claude-haskell
**状态**: ✅ 功能完整,非官方实现

#### 特性
- 支持文本、图片、PDF 输入
- 消息批处理 (Message Batches)
- Token 计数
- 模型信息查询
- **不支持流式响应**

#### 核心 API

```haskell
-- ClaudeAPI.Chat
chat :: ChatRequest -> IO (Either String ChatResponse)

defaultChatRequest :: String -> ChatRequest

-- 媒体支持
defaultIOMediaChatRequest :: FilePath -> String -> IO (Either String ChatRequest)

-- Token 计数
countToken :: CountTokenRequest -> IO (Either String CountTokenResponse)

-- 批处理
createMessageBatch :: MessageBatchRequests -> IO (Either String MessageBatchResponse)
```

**数据类型** (ClaudeAPI.Types):
```haskell
data ChatRequest = ChatRequest
    { model :: String
    , messages :: [RequestMessage]
    , max_tokens :: Int
    , stream :: Maybe Bool  -- 支持字段但未实现
    , system :: Maybe String
    , temperature :: Maybe Double
    }

data RequestMessage = RequestMessage
    { role :: String
    , content :: Either String [RequestMessageContent]
    }

data ChatResponse = ChatResponse
    { id :: String
    , responseContent :: [ResponseMessage]
    , usage :: Usage
    , ...
    }
```

**示例**:
```haskell
import ClaudeAPI

main :: IO ()
main = do
    chatResponse <- chat $ defaultChatRequest "What is Haskell?"
    case chatResponse of
        Left err -> putStrLn $ "Error: " ++ err
        Right resp -> do
            let botReply = responseText $ head $ responseContent resp
            putStrLn $ "Claude: " ++ botReply
```

---

### 2.2 claude-api (Elvecent)

**仓库**: https://github.com/elvecent/claude-api
**许可**: AGPL-3.0
**状态**: ⚠️ 早期开发阶段 (8 commits, April 2024)

#### 模块结构
```
src/
├── Anthropic/Claude/API.hs      -- Servant API 定义
├── Anthropic/Claude/Client.hs   -- 客户端实现
├── Anthropic/Claude/Types.hs    -- 数据类型
└── Data/Aeson/Utils.hs          -- JSON 工具
```

---

### 2.3 claudius (rescrv)

**仓库**: https://github.com/rescrv/claudius
**状态**: ⭐ 8 stars, SDK 实现
**特性**: 可能包含更多高级功能,需要进一步调查

---

## 3. HTTP 客户端库 (用于 LLM API)

### 3.1 http-client + http-client-tls

**推荐用于**: 直接 HTTP 调用,流式处理

```haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://api.openai.com/v1/chat/completions"
    
    let req = request
            { method = "POST"
            , requestHeaders = 
                [ ("Authorization", "Bearer " <> apiKey)
                , ("Content-Type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode payload)
            }
    
    withResponse req manager $ \response -> do
        -- 流式处理
        let body = responseBody response
        processStream body
```

---

### 3.2 servant-client (推荐)

**适用于**: 类型安全的 API 客户端

```haskell
import Servant.API
import Servant.Client

type ChatAPI = "v1" :> "chat" :> "completions"
    :> Header' '[Required] "Authorization" Text
    :> ReqBody '[JSON] ChatRequest
    :> Post '[JSON] ChatResponse

chatAPI :: Proxy ChatAPI
chatAPI = Proxy

createChat :: Text -> ChatRequest -> ClientM ChatResponse
createChat = client chatAPI
```

---

### 3.3 req

**特性**: 高级 HTTP 客户端,更简洁的 API

```haskell
import Network.HTTP.Req

response <- req POST 
    (https "api.openai.com" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson chatRequest)
    jsonResponse
    (header "Authorization" ("Bearer " <> apiKey))
```

---

## 4. 流式响应 (SSE) 处理

### 4.1 Servant SSE 支持

**servant-client-core** 提供 SSE 类型:

```haskell
-- Servant.Client.Core.ServerSentEvents
data EventMessage = EventMessage { ... }
newtype EventStreamT m a = EventStreamT { ... }

-- Servant.API.ContentTypes
data EventStream

-- 用法
type StreamAPI = 
    Stream 'GET 200 NoFraming EventStream (SourceIO ByteString)
```

---

### 4.2 手动 SSE 解析 (openai 库方式)

**核心逻辑**:
```haskell
-- 1. 使用 http-client withResponse 获取流
HTTP.Client.withResponse request manager $ \response -> do
    lineBufRef <- IORef.newIORef ByteString.empty
    eventBufRef <- IORef.newIORef []
    
    let loop = do
        chunk <- HTTP.Client.brRead (responseBody response)
        if ByteString.null chunk
            then flushEvent
            else do
                -- 分割行,处理 "data: " 前缀
                -- 空行触发事件 flush
                processLines chunk
                loop
    loop

-- 2. 解析 SSE 格式
-- data: {"id":"...", "choices":[...]}
-- data: [DONE]
```

---

### 4.3 conduit / pipes (流处理)

```haskell
-- conduit 方式
import Conduit

processStream :: ConduitT () ByteString IO () -> IO ()
processStream source = runConduit $
    source
    .| linesUnboundedC
    .| filterC (isPrefixOf "data: ")
    .| mapC (drop 6)  -- 移除 "data: "
    .| mapMC decodeEvent
    .| mapM_C handleEvent

-- pipes 方式
import Pipes
import qualified Pipes.Prelude as P

processStream :: Producer ByteString IO () -> IO ()
processStream source =
    runEffect $ source
        >-> P.map parseLine
        >-> P.filter isDataLine
        >-> P.mapM handleEvent
```

---

## 5. 推荐组合方案

### 方案 A: 生产级 (OpenAI)

```haskell
-- package.yaml / .cabal
dependencies:
  - openai >= 2.2  -- MercuryTechnologies
  - http-client-tls
  - aeson
  - text

-- 代码
import qualified OpenAI.V1 as OpenAI
import qualified OpenAI.V1.Chat.Completions as Chat

-- 流式响应内置支持
```

---

### 方案 B: 自建灵活方案

```haskell
-- dependencies
dependencies:
  - servant
  - servant-client
  - http-client
  - http-client-tls
  - aeson
  - conduit  -- 或 pipes

-- 实现自定义 API 类型
-- 参考 openai 库的实现方式
```

---

### 方案 C: Claude API

```haskell
-- cabal.project
source-repository-package
  type: git
  location: https://github.com/T0mLam/claude-haskell.git

-- 代码
import ClaudeAPI

-- 注意: 不支持流式,仅批量请求
```

---

## 6. 代码模式总结

### 6.1 非流式请求

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson

data ChatRequest = ChatRequest
    { model :: Text
    , messages :: [Message]
    , temperature :: Double
    } deriving (Generic, ToJSON)

data ChatResponse = ChatResponse
    { id :: Text
    , choices :: [Choice]
    } deriving (Generic, FromJSON)

callLLM :: ChatRequest -> IO ChatResponse
callLLM req = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    
    let request' = request
            { requestHeaders = 
                [ ("Authorization", "Bearer " <> apiKey)
                , ("Content-Type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode req)
            }
    
    response <- httpLbs request' manager
    case eitherDecode (responseBody response) of
        Left err -> error err
        Right result -> return result
```

---

### 6.2 流式请求 (SSE)

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Char8 as BS
import Control.Monad (unless)

streamLLM :: ChatRequest -> (ChatChunk -> IO ()) -> IO ()
streamLLM req onChunk = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    
    let request' = request
            { requestHeaders = 
                [ ("Authorization", "Bearer " <> apiKey)
                , ("Content-Type", "application/json")
                ]
            , requestBody = RequestBodyLBS (encode req{stream = True})
            }
    
    withResponse request' manager $ \response -> do
        let body = responseBody response
        processSSE body onChunk

processSSE :: BodyReader -> (ChatChunk -> IO ()) -> IO ()
processSSE bodyReader onChunk = loop BS.empty
  where
    loop buffer = do
        chunk <- brRead bodyReader
        unless (BS.null chunk) $ do
            let (lines, newBuffer) = splitLines (buffer <> chunk)
            mapM_ processLine lines
            loop newBuffer
    
    processLine line
        | "data: " `BS.isPrefixOf` line = do
            let payload = BS.drop 6 line
            unless (payload == "[DONE]") $ do
                case eitherDecodeStrict payload of
                    Right chunk -> onChunk chunk
                    Left _ -> pure ()
        | otherwise = pure ()
```

---

## 7. 关键技术要点

### SSE 处理要点
1. **格式**: `data: {json}\n\n`
2. **终止**: `data: [DONE]\n\n`
3. **缓冲**: 需要处理跨块的行
4. **错误**: 区分 HTTP 错误和 SSE 解析错误

### HTTP 客户端选择
- **http-client**: 底层控制,流式支持
- **servant-client**: 类型安全,代码生成
- **req**: 简洁 API,快速开发

### JSON 处理
- **aeson**: 标准选择
- `deriving (Generic, FromJSON, ToJSON)` 减少样板代码
- 自定义 `parseJSON` 处理特殊格式

---

## 8. 参考资源

### 官方文档
- OpenAI Streaming: https://platform.openai.com/docs/api-reference/streaming
- Anthropic Messages API: https://docs.anthropic.com/claude/reference/messages_post

### 仓库
- openai (推荐): https://github.com/MercuryTechnologies/openai
- claude-haskell: https://github.com/T0mLam/claude-haskell
- http-client: https://github.com/snoyberg/http-client
- servant: https://github.com/haskell-servant/servant

### 示例
- openai streaming example: https://github.com/MercuryTechnologies/openai/tree/main/examples/chat-completions-stream-example
- langchain-hs (LLM 抽象层): https://github.com/tusharad/langchain-hs
