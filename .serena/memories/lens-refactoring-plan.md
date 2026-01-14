# Lens Refactoring Plan

## Completed Phases

### Phase 1: Add microlens dependencies ✅
- Added `microlens` and `microlens-th` to package.yaml

### Phase 2: Prefix fields with `_` and generate lenses ✅
- All data types now have `_` prefixed fields
- `makeLenses` TH generates lenses for each type
- Modules export lenses, not raw `_` fields

### Phase 3: Replace direct field access with lens operators ✅
- `_field record` → `record ^. field`
- `record { _field = val }` → `record & field .~ val`
- `record { _field = f (_field record) }` → `record & field %~ f`

## Completed Phases

### Phase 4: Smart Constructors ✅
Added smart constructors for all major types:
- `Core/Types.hs`: `makeTool`, `makeAssistantMessage`, `makePartialMessage`, `makeProviderInfo`
- `MCP/Types.hs`: `makeServerConfig`, `makeClientInfo`, `makeInitializeParams`, etc.
- `Effect/MCP.hs`: `makeToolResult`, `makeResource`, `makeResourceContent`
- `Agent/Config.hs`: `makeAgentConfig`, `makeMCPServerConfig`
- Removed `(..)` exports, only export type + lenses + smart constructors

### Phase 5: Prisms for Sum Types ✅
Added manual Prisms using `microlens-pro`:
- `Message`: `_UserMessage`, `_AssistantMsg`, `_SystemMessage`, `_ToolResultMessage`
- `StreamEvent`: `_ContentDelta`, `_ToolCallStart`, `_ToolCallDelta`, `_Ping`
- `StreamResult`: `_StreamCompleted`, `_StreamInterrupted`, `_StreamFailed`
- Updated `extractLastAssistantContent` to use `^?` with `_AssistantMsg` prism

## Remaining Refactoring Opportunities

### Phase 6: Map Operations with `at`/`ix` ✅
Completed lens-based map access in `LLM/Streaming.hs` and `Agent/Context.hs`.

## Next Phases

### Phase 7: Advanced Lens Patterns (PENDING)
- **Traversals**: Use `traverse`, `each`, `_Just` for batch operations on lists/maybes
- **Composable optics**: Chain lenses for nested access (e.g., `config . server . port`)
- **Folds**: Use `toListOf`, `has`, `hasn't` for queries

### Phase 8: Polysemy State Integration (PENDING)
- Use lens operators with `State` effect: `modify (& field %~ f)`, `gets (^. field)`
- Replace manual pattern matching in effect handlers with lens-based access

### Phase 9: Cleanup & Documentation (PENDING)
- Remove any remaining direct field access patterns
- Add Haddock documentation for all smart constructors
- Ensure consistent export patterns across all modules
