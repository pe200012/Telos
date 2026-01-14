# Telos CLI 实现 (2026-01-11)

## 新增模块

### `Telos.CLI.Config`
- `CliConfig`: CLI 配置类型（model, maxIterations, systemPrompt, mcpServers, streamingEnabled）
- `McpServerEntry`: MCP Server 配置条目
- `loadConfig`: 从 `~/.config/telos/config.json` 加载配置
- `configFilePath`: 获取配置文件路径

### `Telos.CLI.LazyServerManager`
- `LazyServerManager`: 懒加载 MCP Server 管理器
- `registerServer`: 注册但不连接
- `getOrConnectServer`: 按需连接
- `aggregateToolsLazy`: 懒加载聚合工具
- `ServerStatus`: Registered | Connected | Failed

### `Telos.CLI.Repl`
- `ReplState`: REPL 状态
- `runRepl`: REPL 主循环
- `ReplCommand`: /quit, /clear, /tools, /servers, /help

## 配置文件格式

```json
{
  "model": "gpt-4o",
  "maxIterations": 20,
  "systemPrompt": "You are a helpful assistant.",
  "mcpServers": [
    {
      "name": "filesystem",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
    }
  ],
  "streamingEnabled": true
}
```

## 使用方式

```bash
# 构建
stack build

# 运行 CLI
stack run telos-exe

# REPL 命令
/quit, /q    - 退出
/clear       - 清空对话历史
/tools       - 列出可用工具
/servers     - 显示 MCP server 状态
/help, /h    - 显示帮助
```

## 测试状态
- 68 个单元测试通过
- 集成测试需要 MCP server 环境
