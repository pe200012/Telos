# Design Log #011: Configuration System

**Date**: 2026-01-12
**Status**: Planning → Implementation

## Background

Telos needs a comprehensive configuration system inspired by OpenCode's `opencode.json`. Currently, config is minimal (model, maxIterations, mcpServers) and JSON-only from `~/.config/telos/config.json`.

## Problem

1. No provider configuration (API keys, base URLs)
2. No permission system for tools
3. No project-local config
4. No env var interpolation
5. Limited DCP configuration

## Design

### Config Loading Priority
```
defaults → global (~/.config/telos/telos.yaml) → project (./telos.yaml) → env vars
```

### Core Types
- `TelosConfig`: Root config with model, providers, mcp, permissions, compaction
- `ProviderConfig`: apiKey, baseURL, timeout
- `McpConfig`: local (command/args) or remote (url/headers)
- `Permission`: Ask | Allow | Deny
- `CompactionConfig`: auto, prune, threshold

### Key Files
- `src/Telos/Config/Types.hs` - Type definitions
- `src/Telos/Config/Load.hs` - Loading & merging
- `src/Telos/Config/Permission.hs` - Permission checking

### Plan Location
`docs/plans/2026-01-12-config-system.md`

## Implementation Results

(To be filled during implementation)
