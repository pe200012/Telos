# Telos Next Steps

Current Status: All core designs (#001 - #005) have been implemented. The project has LLM-MCP architecture, agent loops, streaming, persistence, and dynamic system prompts.

## Upcoming Priorities

1. **Agent Orchestration (Subagents)**
   - Implement delegation mechanism where one agent can spawn specialized subagents.
   - Define subagent lifecycle and context sharing.

2. **Built-in Tool Expansion**
   - Implement common tools directly (File read/write, Web search) to reduce MCP overhead.
   - Standardize tool error handling.

3. **Multi-Model Support**
   - Add Anthropic Claude 3.5 Sonnet support.
   - Support for local LLMs via Ollama/Llama.cpp.

4. **Testing & Robustness**
   - Add property-based testing for prompt building.
   - Integration tests for the agent loop.

5. **Configuration System**
   - Support `~/.telos/config.yaml` for defaults (model, MCP servers, iteration limits).
