# Design #007: Undo/Redo Commands

## Background

Users need the ability to undo changes made by the AI agent and redo them if needed. This is critical for:
- Reverting unwanted file modifications
- Exploring different approaches without fear
- Recovering from mistakes

## Problem

When the agent makes file changes (via `edit`, `write`, `bash` tools), users need a way to:
1. Revert to a previous conversation state
2. Restore files to their state before the reverted messages
3. Optionally redo the reverted changes

## Design

### Approach: Git-based Snapshots

Use the working directory's git repository to track file states:
- Take a snapshot (commit hash or stash) before each assistant turn
- Store snapshot reference with each message
- On undo: restore files from snapshot, remove messages from history
- On redo: replay stored messages, restore file changes

### Data Structures

```haskell
-- Snapshot reference stored with messages
data Snapshot = Snapshot
  { _snapHash     :: Text        -- Git commit/tree hash
  , _snapFiles    :: [FilePath]  -- Files changed in this turn
  , _snapPatches  :: Text        -- Unified diff for redo
  } deriving (Eq, Show, Generic)

-- Undo state in ReplState
data UndoState = UndoState
  { _usRedoStack :: [(Message, Snapshot)]  -- Undone messages for redo
  , _usSnapshots :: Map Int Snapshot       -- Message index -> snapshot
  } deriving (Eq, Show, Generic)
```

### Commands

**`/undo [n]`**
- Undo last `n` assistant turns (default: 1)
- Revert files to snapshot state
- Move messages to redo stack

**`/redo [n]`**
- Redo last `n` undone turns (default: 1)
- Restore file changes
- Move messages back to history

### Implementation Plan

#### Phase 1: Snapshot Module
Create `src/Telos/Snapshot.hs`:
- `takeSnapshot :: IO (Maybe Text)` - Get current git HEAD or create stash
- `restoreSnapshot :: Text -> [FilePath] -> IO ()` - Checkout files from snapshot
- `getChangedFiles :: Text -> Text -> IO [FilePath]` - Files changed between commits
- `getDiff :: Text -> Text -> IO Text` - Get unified diff

#### Phase 2: Message Tracking
Modify `AgentContext` or `ReplState`:
- Add `UndoState` field
- Track snapshots per message
- Store changed files with each assistant response

#### Phase 3: Commands
Add to `Repl.hs`:
- `handleUndo :: Int -> ReplM ()`
- `handleRedo :: Int -> ReplM ()`

### Simplified MVP

For MVP, use a simpler approach:
1. Track message history index before each turn
2. On `/undo`: truncate history, run `git checkout .` (if in git repo)
3. On `/redo`: not supported in MVP (can add later)

## Trade-offs

| Decision | Pros | Cons |
|----------|------|------|
| Git-based | Leverages existing VCS, accurate | Requires git repo |
| Per-message snapshots | Fine-grained undo | Storage overhead |
| MVP without redo | Simpler implementation | Less functionality |

## Examples

```
user> Fix the bug in main.hs
assistant> [modifies main.hs]

user> /undo
Undone 1 turn. Files restored: main.hs

user> Try a different approach
assistant> [modifies main.hs differently]
```

---
**Status**: Draft
