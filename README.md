# Telos

Telos is a Haskell CLI LLM client built around Polysemy and Conduit.

## Configuration

Telos loads configuration from the XDG config path:

- `~/.config/telos/config.toml`

If the file does not exist, Telos will create it with defaults on first run.

Example `config.toml`:

```toml
api_key = ""
base_url = "https://open.bigmodel.cn/api/paas/v4/chat/completions"
model = "glm-4.7-flash"
temperature = 0.7
```

You can set `ZHIPUAI_API_KEY` to override `api_key` at runtime.
