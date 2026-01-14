# Design Log #012: Advanced Lens Refactoring

## Background

Telos has been progressively adopting lenses via `microlens` and `microlens-th`. Phases 1-6 of the lens refactoring plan are complete:
- ✅ Dependencies added
- ✅ Fields prefixed with `_` and lenses generated
- ✅ Direct field access replaced with lens operators
- ✅ Smart constructors for all major types
- ✅ Prisms for sum types (Message, StreamEvent, StreamResult)
- ✅ Map operations with `at`/`ix`

Remaining work focuses on advanced lens patterns (Phase 7) and Polysemy State integration (Phase 8).

---

## Problem

The codebase still has opportunities to leverage advanced lens patterns for cleaner, more composable code:

1. **Traversals**: Lists and Maybes are still using manual pattern matching in many places
2. **Composable optics**: Nested record access sometimes chains multiple lens operations manually
3. **Folds**: Queries on collections use manual recursion or list comprehensions
4. **Polysemy State integration**: State effect handlers use manual field extraction instead of lens-based access
5. **Code duplication**: Similar patterns of list/option processing appear repeatedly

This results in:
- Verbose boilerplate for common operations
- Less composable data transformation patterns
- Missed opportunities for point-free style
- Inconsistent approaches across modules

---

## Questions and Answers

**Q1: Which modules benefit most from traversals?**
A: Modules with complex list/option processing:
- `Agent/Context.hs`: Managing `Vector Message`, `Map ToolName ToolState`
- `LLM/Streaming.hs`: Processing `StreamEvent` lists
- `MCP/Types.hs`: Nested optionals in `ServerConfig`

**Q2: How deep should we go with composable optics?**
A: Balance between readability and composition:
- ✅ Use for 2-3 level nesting (e.g., `config . mcp . servers`)
- ❌ Avoid for 4+ levels unless clearly named intermediate optics
- Create specialized optics for frequently-used deep paths

**Q3: Should we add `microlens-ghc` for GHC-specific prisms?**
A: Not currently needed. Our sum types are simple enough that manual prisms work. Consider if we add GHC-specific types (e.g., `MaybeT`, `ExceptT`) in the future.

**Q4: How do we balance lens usage with readability for new contributors?**
A:
- Use lenses consistently but add inline comments for complex optics
- Prefer explicit `^.`/`.~` over cryptic compositions
- Document complex optics in module Haddock
- Keep simple field access simple (2-3 hops max)

**Q5: Should we replace ALL manual pattern matching?**
A: No. Pattern matching is clearer for:
- Simple destructuring (`case x of Just v -> ...`)
- Exhaustive pattern matches on small ADTs
- Error handling with specific cases
- Use optics for transformation/traversal, not just extraction

---

## Design

### Phase 7: Advanced Lens Patterns

#### 7.1 Traversals for Lists and Maybes

**Target Modules**: `Agent/Context.hs`, `LLM/Streaming.hs`, `MCP/Types.hs`

**Patterns to adopt**:

```haskell
-- BEFORE: Manual filter and map
filterUserMessages :: Vector Message -> Vector Message
filterUserMessages = Vector.filter isUserMessage
  where
    isUserMessage (UserMessage _) = True
    isUserMessage _ = False

-- AFTER: Traversal-based
filterUserMessages :: Vector Message -> Vector Message
filterUserMessages = Vector.toListOf (traversed . filtered (has _UserMessage))

-- OR with prism composition
filterUserMessages :: Vector Message -> Vector Message
filterUserMessages = Vector.toListOf (traversed . _UserMessage)

-- Example: Update all tool names in a list
updateToolNames :: [Tool] -> ToolName -> [Tool]
updateToolNames tools newName = tools & each . name .~ newName

-- Example: Collect all assistant messages
getAssistantMessages :: [Message] -> [AssistantMsg]
getAssistantMessages = toListOf (traversed . _AssistantMsg)

-- Example: Filter and transform in one pass
activeTools :: Map ToolName Tool -> Map ToolName Tool
activeTools = Map.filter (^. isActive)
```

**Common traversals to add**:

```haskell
-- Agent/Context.hs specific
type MessageTraversal = Traversal' Context Message
messages :: MessageTraversal

-- Extract all user messages
userMessages :: Traversal' Context UserMsg
userMessages = messages . _UserMessage

-- Update all assistant messages
updateAssistants :: (AssistantMsg -> AssistantMsg) -> Context -> Context
updateAssistants f ctx = ctx & messages %~ mapMaybe (preview _AssistantMsg >>> fmap f)
```

#### 7.2 Composable Optics for Nested Access

**Create specialized optics for common deep paths**:

```haskell
-- src/Telos/Lens.hs (new module)
module Telos.Lens where

import Telos.Config.Types
import Telos.Agent.Types
import Telos.MCP.Types

-- Nested access helpers
configOpenAIProvider :: Lens' TelosConfig (Maybe ProviderConfig)
configOpenAIProvider = providers . at "openai"

configMCPServers :: Traversal' TelosConfig McpConfig
configMCPServers = mcp . _Local . servers

configMaxIterations :: Lens' TelosConfig Int
configMaxIterations = agent . maxIterations

-- Agent-specific nested optics
contextMessages :: Lens' Context (Vector Message)
contextMessages = messages

contextUserMessages :: Fold Context UserMsg
contextUserMessages = messages . folded . _UserMessage

contextAssistantMessages :: Fold Context AssistantMsg
contextAssistantMessages = messages . folded . _AssistantMsg

-- Tool state optics
toolIsActive :: Lens' Tool Bool
toolIsActive = isActive

toolConfig :: Lens' Tool ToolConfig
toolConfig = config

-- MCP server optics
serverIsActive :: Lens' ServerConfig Bool
serverIsActive = isActive

serverTools :: Traversal' ServerConfig Tool
serverTools = tools . traversed
```

#### 7.3 Folds for Queries

**Replace manual queries with fold-based optics**:

```haskell
-- BEFORE: Manual recursion
hasUserMessages :: Context -> Bool
hasUserMessages ctx = any isUserMessage (ctx ^. messages)
  where
    isUserMessage (UserMessage _) = True
    isUserMessage _ = False

-- AFTER: Fold-based
hasUserMessages :: Context -> Bool
hasUserMessages = has (messages . folded . _UserMessage)

-- Example: Count tools
countActiveTools :: [Tool] -> Int
countActiveTools = lengthOf (each . filtered (^. isActive))

-- Example: Any/All checks
anyToolWithError :: [Tool] -> Bool
anyToolWithError = has (each . status . _Error)

allToolsHealthy :: [Tool] -> Bool
allToolsHealthy = hasn't (each . status . _Error)

-- Example: Collect tool names
getToolNames :: [Tool] -> [ToolName]
getToolNames = toListOf (each . name)

-- Example: Find by predicate
findToolByName :: ToolName -> [Tool] -> Maybe Tool
findToolByName tName = preview (each . filtered (^. name .~ tName))
```

### Phase 8: Polysemy State Integration

**Target Modules**: Effect handlers in `Agent/Loop.hs`, `LLM/Streaming.hs`

**Pattern**: Use lens operators with `State` effect instead of manual pattern matching:

```haskell
-- BEFORE: Manual field extraction and update
runAgentState :: Member (State AgentState) r => Sem r a
runAgentState = do
  st <- get
  let newCtx = updateContext (agentContext st)
  put $ st { agentContext = newCtx }

-- AFTER: Lens-based state update
runAgentState :: Member (State AgentState) r => Sem r a
runAgentState = do
  modify (agentContext %~ updateContext)
  -- OR:
  gets (^. agentContext) >>= \ctx -> ...

-- Example: Update tool state
updateTool :: ToolName -> (Tool -> Tool) -> Member (State Context) r => Sem r ()
updateTool tName f = modify (tools . ix tName %~ f)

-- Example: Add message
addMessage :: Message -> Member (State Context) r => Sem r ()
addMessage msg = modify (messages %~ Vector.snoc msg)

-- Example: Update provider config
updateProviderConfig :: Provider -> Member (State TelosConfig) r => Sem r ()
updateProviderConfig provider = modify (providers . at provider .~ Just newConfig)

-- Example: Conditional update
ifToolActive :: ToolName -> Sem r () -> Member (State Context) r => Sem r ()
ifToolActive tName action = do
  active <- gets (has (tools . ix tName . isActive))
  when active action
```

---

## Implementation Plan

### Phase 7: Advanced Lens Patterns

#### 7.1 Create `src/Telos/Lens.hs` module
- Define specialized composable optics for common nested access
- Export frequently-used traversals and folds
- Add Haddock documentation with examples

#### 7.2 Update `Agent/Context.hs`
- Replace manual list filtering with `traversed . filtered` + prisms
- Add folds for queries (hasUserMessages, countTools, etc.)
- Use `each`, `_Just` for batch updates

#### 7.3 Update `LLM/Streaming.hs`
- Replace manual `StreamEvent` processing with traversals
- Use `toListOf` to extract specific event types
- Add folds for stream status queries

#### 7.4 Update `MCP/Types.hs` and related modules
- Use `_Just` prism for optionals
- Apply `each` for batch updates on lists
- Add specialized optics for server/tool states

### Phase 8: Polysemy State Integration

#### 8.1 Update `Agent/Loop.hs`
- Replace manual state updates with lens operators
- Use `modify (& field %~ f)` pattern
- Use `gets (^. field)` for extraction

#### 8.2 Update other effect handlers
- Apply lens patterns consistently across State effect handlers
- Extract common state update patterns

### Phase 9: Cleanup & Documentation

#### 9.1 Remove remaining direct field access
- Search for `^. _` pattern and replace with proper lenses
- Remove unused `_` field exports from modules

#### 9.2 Add Haddock documentation
- Document all smart constructors with examples
- Document complex optics in `Lens.hs` module
- Add usage examples for common patterns

#### 9.3 Ensure consistent exports
- All modules export: type + lenses + smart constructors (no raw `_` fields)
- Remove `(..)` exports from ADTs

---

## Examples

### Example 1: Batch Update Tools with Traversals

```haskell
-- BEFORE: Manual recursion
activateAllTools :: [Tool] -> [Tool]
activateAllTools = map (\t -> t { _isActive = True })

-- AFTER: Traversal-based
activateAllTools :: [Tool] -> [Tool]
activateAllTools tools = tools & each . isActive .~ True

-- Example: Conditional update
activateToolByPrefix :: Text -> [Tool] -> [Tool]
activateToolByPrefix prefix tools =
  tools & each . filtered (^. name . to (Text.isPrefixOf prefix)) . isActive .~ True
```

### Example 2: Query with Folds

```haskell
-- BEFORE: Manual recursion
hasFailedTools :: Context -> Bool
hasFailedTools ctx =
  any (\t -> _status t == Error) (Map.elems $ _tools ctx)

-- AFTER: Fold-based
hasFailedTools :: Context -> Bool
hasFailedTools ctx = has (tools . folded . status . _Error) ctx

-- Example: Collect failed tool names
getFailedToolNames :: Context -> [ToolName]
getFailedToolNames = toListOf (tools . folded . filtered (has $ status . _Error) . name)
```

### Example 3: Composable Optics for Nested Access

```haskell
-- BEFORE: Multiple chained operations
getOpenAIProvider :: TelosConfig -> Maybe ProviderConfig
getOpenAIProvider config = do
  providers <- _providers config
  Map.lookup "openai" providers

-- AFTER: Composable lens
getOpenAIProvider :: TelosConfig -> Maybe ProviderConfig
getOpenAIProvider = preview configOpenAIProvider

-- Usage:
openAIKey :: TelosConfig -> Maybe Text
openAIKey = preview (configOpenAIProvider . _Just . apiKey)
```

### Example 4: Polysemy State with Lenses

```haskell
-- BEFORE: Manual pattern matching
updateAgentConfig :: (AgentConfig -> AgentConfig) -> Sem r ()
updateAgentConfig f = do
  st <- get
  put $ st { _agentConfig = f (_agentConfig st) }

-- AFTER: Lens-based
updateAgentConfig :: (AgentConfig -> AgentConfig) -> Sem r ()
updateAgentConfig f = modify (agentConfig %~ f)

-- Example: Increment iteration count
incrementIteration :: Sem r ()
incrementIteration = modify (iterationCount %~ (+1))

-- Example: Conditional update
ifIterationLimitExceeded :: Sem r Bool
ifIterationLimitExceeded = do
  limit <- gets (^. maxIterations)
  current <- gets (^. iterationCount)
  return $ current >= limit
```

---

## Trade-offs

### Benefits

✅ **More composable code**: Lens operators compose naturally for nested transformations
✅ **Less boilerplate**: Common patterns (filter, map, query) become one-liners
✅ **Type-safe refactoring**: Changing field names/types triggers compile errors at all usages
✅ **Point-free potential**: Can enable cleaner function composition
✅ **Consistency**: Uniform pattern for data access across codebase

### Costs

❌ **Learning curve**: New contributors must understand lens operators (`^.`, `.~`, `%~`, `^?`, etc.)
❌ **Debugging complexity**: Stack traces from deep optics can be harder to follow
❌ **Readability trade-off**: Simple operations can become cryptic (e.g., `toListOf (each . filtered ...)`)
❌ **Overhead**: Lens composition adds slight runtime overhead (usually negligible)

### Decisions

1. **Use lenses for 2+ hops**: Direct field access fine for single field, lenses for composable access
2. **Document complex optics**: Add inline comments for non-obvious compositions
3. **Keep simple patterns simple**: Don't force lenses where plain code is clearer
4. **Prefer readability over cleverness**: Use `^.` and `.~` over cryptic compositions

---

## Implementation Results

### Phase 7: Advanced Lens Patterns - LARGELY COMPLETE

**Analysis**: After reviewing the codebase, found that Telos already extensively uses advanced lens patterns:

✅ **Already implemented**:
- Prism-based message filtering: `msgs ^. _last . _AssistantMsg . Core.amContent . non ""`
- Lens composition for nested access: `config ^. acPromptConfig`, `pstate ^. psToolCache`
- Fold operations: `map formatContentItem (result ^. trContent)`
- `non` combinator for defaults: `amContent . non ""`
- Lens operators in state updates: `pstate & psToolCache %~ Map.insert nextId entry`

**Remaining opportunities** (minimal value):
- Some `filter` operations could use `filtered` traversal, but current code is more readable
- Pattern matching in `case` expressions is appropriate for business logic branching

**Decision**: No significant refactoring needed. Current lens usage follows best practices.

### Phase 8: Polysemy State Integration - NOT APPLICABLE

**Finding**: Telos uses `TVar`/STM for mutable state, not Polysemy `State` effect. Lens patterns are already applied to pure data transformations within IO/STM contexts.

---

## Deviations from Original Design

1. **Phase 7 minimal changes**: Code review showed existing lens usage is already excellent. Only opportunities would reduce readability.
2. **Phase 8 cancelled**: Architecture uses `TVar`/STM, not Polysemy State effect.
3. **Outcome**: Lens refactoring is effectively complete. Focus shifted to `microlens → lens` migration (completed) and documentation.

