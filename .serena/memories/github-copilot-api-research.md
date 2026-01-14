# GitHub Copilot API 研究

## 研究日期: 2026-01-10

---

## API 端点

| 端点 | 用途 |
|------|------|
| `https://api.githubcopilot.com/chat/completions` | 标准 chat |
| `https://api.githubcopilot.com/responses` | Responses API (gpt-5.1-codex) |
| `https://api.githubcopilot.com/mcp/` | MCP |

---

## 认证: OAuth Device Flow

```
CLIENT_ID = "Iv1.b507a08c87ecfe98"

1. POST https://github.com/login/device/code
   Body: {"client_id": "<id>", "scope": "read:user"}
   Response: {device_code, user_code, verification_uri}

2. 用户访问 https://github.com/login/device 输入 user_code

3. 轮询 POST https://github.com/login/oauth/access_token
   Body: {"client_id": "<id>", "device_code": "<code>", 
          "grant_type": "urn:ietf:params:oauth:grant-type:device_code"}
   Response: {access_token}

4. GET https://api.github.com/copilot_internal/v2/token
   Header: Authorization: token <access_token>
   Response: {token: "ghu_xxx", expires_at, endpoints: {api: "..."}}
```

---

## 请求格式 (OpenAI 兼容)

```json
{
  "model": "gpt-4.1",
  "messages": [{"role": "user", "content": "Hello"}],
  "stream": true,
  "tools": [{"type": "function", "function": {...}}]
}
```

### 必需的特殊请求头

```
Authorization: Bearer <api_key>
copilot-integration-id: vscode-chat
editor-version: vscode/1.95.0
editor-plugin-version: copilot-chat/0.26.7
user-agent: GitHubCopilotChat/0.26.7
x-github-api-version: 2025-04-01
X-Initiator: user  # or "agent" if tools in messages
```

---

## 流式响应 (SSE)

```
data: {"id":"chatcmpl-xxx","choices":[{"delta":{"content":"Hi"}}]}
data: {"id":"chatcmpl-xxx","choices":[{"delta":{"content":" there"}}]}
data: [DONE]
```

---

## 工具调用

✅ 完全支持 OpenAI function calling 格式

```json
{
  "tools": [{
    "type": "function",
    "function": {
      "name": "get_weather",
      "description": "...",
      "parameters": {"type": "object", "properties": {...}}
    }
  }],
  "tool_choice": "auto"
}
```

---

## 可用模型

- gpt-4.1 (推荐)
- gpt-4o
- gpt-4-turbo
- gpt-3.5-turbo
- o1, o1-mini, o3-mini
- gpt-5.1-codex (需 Responses API)
- claude-3.5-sonnet, claude-3.7-sonnet (通过 Copilot)

---

## 参考实现

**最佳**: [BerriAI/litellm](https://github.com/BerriAI/litellm/tree/main/litellm/llms/github_copilot)
- `authenticator.py` - OAuth Device Flow
- `chat/transformation.py` - Chat API

**代理方案**: [ericc-ch/copilot-api](https://github.com/ericc-ch/copilot-api)
- `npx copilot-api start` 运行代理
- 暴露 OpenAI 兼容端点 `http://localhost:4141/v1/chat/completions`
