# Haskell MCP (Model Context Protocol) Implementations

## Date: 2026-01-10

## Summary
Found **multiple active Haskell implementations** of MCP (Model Context Protocol) from 2025, plus mature JSON-RPC libraries.

---

## 🎯 MCP Implementations (2025)

### 1. **Tritlo/mcp** - Most Comprehensive ⭐
- **Repository**: https://github.com/Tritlo/mcp
- **Hackage**: https://hackage.haskell.org/package/mcp
- **Author**: Matthias Pall Gissurarson
- **License**: MIT
- **Protocol Version**: MCP 2025-06-18 (latest)
- **Status**: Production-ready, complete implementation

**Key Features**:
- ✅ Complete MCP protocol implementation
- ✅ Both **StdIO** and **HTTP** transports
- ✅ OAuth 2.0 support for HTTP
- ✅ Type-safe design with Aeson
- ✅ All MCP message types (resources, tools, prompts, sampling, elicitation)
- ✅ Server-Sent Events (SSE) for HTTP streaming

**Architecture** (5 modules):
```
MCP.Types       - Core types (Content, Resource, Tool, Prompt, etc.)
MCP.Protocol    - JSON-RPC wrappers, request/response types
MCP.Server      - MCPServer typeclass, MCPServerM monad stack
MCP.Server.StdIO - StdIO transport
MCP.Server.HTTP  - HTTP transport (Servant + Warp)
```

**Dependencies**: 
- aeson, text, containers, bytestring
- servant-server, servant-auth, warp, wai
- stm, async, mtl, transformers

**Code Evidence**: 
- JSON-RPC types: https://github.com/Tritlo/mcp/blob/master/src/MCP/Protocol.hs
- StdIO transport: https://github.com/Tritlo/mcp/blob/master/src/MCP/Server/StdIO.hs

---

### 2. **drshade/haskell-mcp-server** - Template Haskell Focus
- **Repository**: https://github.com/drshade/haskell-mcp-server
- **Hackage**: https://hackage.haskell.org/package/mcp-server
- **Author**: Tom Wells
- **License**: BSD-3-Clause
- **Protocol Version**: MCP 2025-06-18
- **Status**: Fully-featured library

**Key Features**:
- ✅ **Template Haskell derivation** for automatic handler generation
- ✅ High-level and low-level APIs
- ✅ Both STDIO and HTTP Streaming transports
- ✅ Automatic snake_case naming conventions
- ✅ Comprehensive test suite (HSpec)

**Modules**:
```
MCP.Server              - Main server API
MCP.Server.Derive       - TH derivation magic
MCP.Server.JsonRpc      - JSON-RPC implementation
MCP.Server.Protocol     - MCP protocol layer
MCP.Server.Transport.Stdio
MCP.Server.Transport.Http
```

**Unique Approach**: Template Haskell for deriving MCP handlers from ADTs
```haskell
data MyTool = Search { query :: Text } | Order { item :: Text }
handlers = $(deriveToolHandler ''MyTool 'handleTool)
```

---

### 3. **buecking/hs-mcp** - Lightweight Implementation
- **Repository**: https://github.com/buecking/hs-mcp
- **Author**: Bryan Buecking
- **License**: BSD-3-Clause
- **Protocol Version**: 2024-11-05 (earlier version)
- **Status**: Working implementation with examples

**Key Features**:
- ✅ Full MCP protocol support
- ✅ StdIO transport
- ✅ JSON-RPC messaging
- ✅ Client and server implementations
- ✅ Example echo server and client

**Modules**:
```
Network.MCP.Types
Network.MCP.Transport.StdIO
Network.MCP.Server
Network.MCP.Client
```

**Dependencies**: Minimal - aeson, stm, async, text, bytestring

---

## 🔧 JSON-RPC Libraries

### 1. **GaloisInc/argo** - Production JSON-RPC Framework ⭐
- **Repository**: https://github.com/GaloisInc/argo
- **Commit SHA**: f1210073ea0c4fff94f9a2961d2b7f35c136f0d0
- **License**: BSD-3-Clause
- **Status**: Battle-tested (used by Cryptol and SAW)

**Key Features**:
- ✅ Explicit state management with rollbacks
- ✅ Multiple transports: stdio, socket, HTTP
- ✅ Filesystem caching
- ✅ Python bindings
- ✅ Comprehensive documentation

**Core API**:
```haskell
-- From Argo.hs
App, AppMethod
mkApp, command, query, notification
serveStdIO, serveHttp
JSONRPCException, raise, makeJSONRPCException
```

**Evidence**: https://github.com/GaloisInc/argo/blob/f1210073ea0c4fff94f9a2961d2b7f35c136f0d0/argo/src/Argo.hs#L21-L86

**Usage in Cryptol**: https://github.com/GaloisInc/cryptol/blob/master/cryptol-remote-api/src/CryptolServer.hs

---

### 2. **haskell/lsp** - Language Server Protocol
- **Repository**: https://github.com/haskell/lsp
- **Commit SHA**: c4ca81bcc47ec23de476bf841d97fce2564f318c
- **Status**: Mature, widely used

**Relevant for**: JSON-RPC patterns in LSP (similar to MCP)

**Key Message Types**:
```haskell
data RequestMessage = RequestMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text
  , _method :: Text
  , _params :: Maybe Value
  }

data ResponseMessage = ResponseMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text |? Null
  , _result :: Maybe Value
  , _error :: Maybe ResponseError
  }
```

**Evidence**: https://github.com/haskell/lsp/blob/c4ca81bcc47ec23de476bf841d97fce2564f318c/lsp-types/src/Language/LSP/Protocol/Message/Types.hs#L41-L98

---

### 3. Other JSON-RPC Libraries on Hackage

1. **json-rpc** (v1.1.1)
   - URL: https://hackage.haskell.org/package/json-rpc
   - Fully-featured JSON-RPC 2.0 library
   - Updated: 2025-05-09

2. **servant-jsonrpc** (v1.2.0)
   - URL: https://hackage.haskell.org/package/servant-jsonrpc
   - JSON-RPC messages + Servant endpoints
   - BSD-3-Clause

3. **jsonrpc-tinyclient** (v1.0.1.0)
   - URL: https://hackage.haskell.org/package/jsonrpc-tinyclient
   - Minimalistic JSON-RPC client

---

## 📊 Implementation Patterns

### JSON-RPC Message Structure (Common Pattern)
All implementations follow similar structure:

```haskell
-- Request ID (supports string, number, or null)
data RequestId 
  = RequestIdText Text
  | RequestIdNumber Int
  | RequestIdNull

-- JSON-RPC Request
data JsonRpcRequest = JsonRpcRequest
  { requestJsonrpc :: Text        -- Always "2.0"
  , requestId :: RequestId
  , requestMethod :: Text
  , requestParams :: Maybe Value
  }

-- JSON-RPC Response
data JsonRpcResponse = JsonRpcResponse
  { responseJsonrpc :: Text
  , responseId :: RequestId
  , responseResult :: Maybe Value
  , responseError :: Maybe JsonRpcError
  }

-- JSON-RPC Error
data JsonRpcError = JsonRpcError
  { errorCode :: Int
  , errorMessage :: Text
  , errorData :: Maybe Value
  }
```

### Transport Patterns

**StdIO Transport** (from Tritlo/mcp and buecking/hs-mcp):
```haskell
-- Line-buffered JSON messages over stdin/stdout
handleMessage :: ByteString -> MCPServerM (Maybe ())
handleMessage input = do
  case decode (LBS.fromStrict input) of
    Just msg -> handleRequest msg
    Nothing -> sendError parseError
```

**HTTP Transport** (Servant-based):
- POST endpoint for JSON-RPC requests
- Server-Sent Events (SSE) for streaming
- OAuth 2.0 authentication support

---

## 🎓 Recommended Approach for New Implementation

### Option A: Use Existing MCP Library
**Best Choice**: `Tritlo/mcp` (mcp package)
- Most complete and up-to-date (2025-06-18)
- Both transports supported
- Production-ready
- Good documentation

### Option B: Use JSON-RPC Library + Custom MCP Layer
**Best Choice**: `GaloisInc/argo`
- Proven in production (Cryptol, SAW)
- Clean abstraction for state management
- Well-documented patterns

### Option C: Build from Scratch
Learn from these implementations:
1. JSON-RPC layer: Study `haskell/lsp` or `drshade/haskell-mcp-server/JsonRpc.hs`
2. MCP protocol: Study `Tritlo/mcp/Protocol.hs`
3. Transport: Study StdIO implementations from all three

---

## 📦 Hackage Packages Summary

| Package | Version | Updated | Protocol |
|---------|---------|---------|----------|
| `mcp` | 0.3.0.0 | 2025-06-12 | MCP 2025-06-18 |
| `mcp-server` | 0.1.0.15 | 2025-08-13 | MCP 2025-06-18 |
| `json-rpc` | 1.1.1 | 2025-05-09 | JSON-RPC 2.0 |
| `servant-jsonrpc` | 1.2.0 | 2024-09-28 | JSON-RPC 2.0 |

---

## 🔍 Search Keywords Used

Successful searches:
- "import Language.LSP" (found haskell/lsp examples)
- "Argo.JsonRpc", "import Argo" (found GaloisInc/argo usage)
- "data RequestMessage", "data ResponseMessage" (found JSON-RPC patterns)

Web searches:
- "Haskell MCP Model Context Protocol implementation 2025"
- "Haskell JSON-RPC library jsonrpc argo 2025"

GitHub code searches discovered multiple real-world examples in:
- Unison LSP implementation
- Swarm game LSP
- Cryptol remote API
