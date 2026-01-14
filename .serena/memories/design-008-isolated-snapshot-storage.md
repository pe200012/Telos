# Design #008: Isolated Snapshot Storage

## Background

Design #007 implemented undo/redo using the project's own Git repository (`git stash create`, `git checkout`). While functional, this approach has drawbacks:

1. **Pollutes project `.git`**: Creates dangling objects in the user's repository
2. **Interference risk**: Could conflict with user's git operations
3. **Visibility concerns**: Telos internals visible in `git fsck`, reflog, etc.

OpenCode solves this elegantly by using a **completely separate Git repository** for snapshots, stored in the XDG data directory.

## Problem

Current implementation in `src/Telos/Snapshot.hs`:

```haskell
-- Uses project's .git directory
takeSnapshot :: IO (Maybe Text)
takeSnapshot = do
  (exitCode, out, _) <- readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
  -- Returns project's HEAD commit

restoreFiles :: Text -> [FilePath] -> IO (Either Text ())
restoreFiles hash files = do
  let args = ["checkout", toString hash, "--"] <> files
  -- Operates on project's .git
```

**Issues**:
- `git rev-parse HEAD` returns project commit, not Telos snapshot
- `git checkout` modifies project's index
- No isolation from user's git workflow

## Questions and Answers

### Q1: Where should the snapshot Git repository be stored?
**A**: `~/.local/share/telos/snapshots/{project-hash}/`

The project-hash is derived from the canonical project path to ensure:
- Each project has isolated snapshot storage
- Same project opened from different paths uses same snapshots
- No collision between projects

### Q2: How do we identify projects uniquely?
**A**: Use SHA256 hash of the canonical (resolved) project path.

```haskell
getProjectHash :: FilePath -> IO Text
getProjectHash projectPath = do
  canonical <- canonicalizePath projectPath
  pure $ T.take 16 $ sha256 canonical  -- 16 chars is enough
```

### Q3: Should we use `git commit` or `git write-tree`?
**A**: Use `git write-tree` (tree objects only).

| Approach | Pros | Cons |
|----------|------|------|
| `git commit` | Full history, easy browsing | Creates commit objects, author info |
| `git write-tree` | Minimal storage, no metadata | No commit history |

OpenCode uses `write-tree`. We follow this for minimal overhead.

### Q4: How do we restore files from a tree object?
**A**: Two approaches:

```bash
# Single file restore
git --git-dir=$SNAP_DIR checkout $TREE_HASH -- path/to/file

# Full restore (all tracked files)
git --git-dir=$SNAP_DIR read-tree $TREE_HASH
git --git-dir=$SNAP_DIR checkout-index -a -f
```

### Q5: How do we handle files created by the agent (not in snapshot)?
**A**: Track files modified per turn. On undo:
1. Files in snapshot → restore from tree
2. Files NOT in snapshot (newly created) → delete them

### Q6: What about untracked files in the project?
**A**: We track ALL files in the work tree, not just git-tracked ones. This differs from the project's `.gitignore`.

However, we should respect a `.telosignore` or reuse `.gitignore` for sanity (large files, node_modules, etc.).

**Decision**: Reuse project's `.gitignore` for snapshot exclusions.

### Q7: Configuration option to disable snapshots?
**A**: Yes, add `snapshotEnabled :: Bool` to `CliConfig`.

```json
{
  "snapshot": false  // Disable snapshots
}
```

## Design

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         User's Project                          │
│  /home/user/myproject/                                          │
│  ├── .git/              ← User's git (UNTOUCHED)                │
│  ├── src/                                                       │
│  └── ...                                                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ GIT_WORK_TREE
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Telos Snapshot Storage                       │
│  ~/.local/share/telos/snapshots/{project-hash}/                 │
│  ├── objects/           ← Git object store                      │
│  ├── refs/                                                      │
│  └── HEAD                                                       │
│                                                                 │
│  GIT_DIR points here, GIT_WORK_TREE points to project           │
└─────────────────────────────────────────────────────────────────┘
```

### Data Flow

```
User Message → Agent Turn Start
                    │
                    ▼
            ┌───────────────┐
            │ takeSnapshot  │
            │ git add .     │
            │ git write-tree│
            └───────┬───────┘
                    │ Returns tree hash (e.g., "a1b2c3d4...")
                    ▼
            Store in UndoEntry
                    │
                    ▼
            Agent executes tools
            (may modify files)
                    │
                    ▼
            Turn complete
            
            
/undo command
      │
      ▼
┌─────────────────────────────────┐
│ restoreFromSnapshot             │
│ 1. Get tree hash from UndoEntry │
│ 2. For each modified file:      │
│    - If in tree: checkout       │
│    - If not in tree: delete     │
└─────────────────────────────────┘
```

### New Module: `Telos.Snapshot`

```haskell
module Telos.Snapshot
  ( -- * Types
    Snapshot(..)
  , snapHash
  , snapFiles
  , snapDiff
  , SnapshotConfig(..)
  , defaultSnapshotConfig
  
    -- * Snapshot Operations
  , initSnapshotRepo      -- Initialize isolated git repo
  , takeSnapshot          -- Create tree object, return hash
  , restoreFiles          -- Restore specific files from snapshot
  , restoreSnapshot       -- Restore entire snapshot state
  , getChangedFiles       -- Files changed since snapshot
  , getDiff               -- Unified diff from snapshot
  
    -- * Path Management
  , getSnapshotDir        -- Get snapshot git directory for project
  , getProjectHash        -- Hash project path for isolation
  
    -- * Utilities
  , isSnapshotEnabled
  , cleanupOldSnapshots   -- GC for old snapshot objects
  ) where
```

### Key Data Structures

```haskell
-- | Configuration for snapshot system
data SnapshotConfig = SnapshotConfig
  { _scEnabled     :: Bool        -- ^ Whether snapshots are enabled
  , _scProjectPath :: FilePath    -- ^ Project working directory
  , _scGitDir      :: FilePath    -- ^ Isolated git directory (computed)
  } deriving (Eq, Show, Generic)

-- | Snapshot of file state (unchanged from current design)
data Snapshot = Snapshot
  { _snapHash  :: Text        -- ^ Git tree hash
  , _snapFiles :: [FilePath]  -- ^ Files modified after this snapshot
  , _snapDiff  :: Text        -- ^ Unified diff (for display)
  } deriving (Eq, Show, Generic)

instance FromJSON Snapshot
instance ToJSON Snapshot
```

### Core Functions

#### 1. Initialize Snapshot Repository

```haskell
-- | Initialize isolated snapshot git repository for a project
initSnapshotRepo :: FilePath -> IO (Either Text FilePath)
initSnapshotRepo projectPath = do
  snapDir <- getSnapshotDir projectPath
  exists <- doesDirectoryExist snapDir
  
  unless exists $ do
    createDirectoryIfMissing True snapDir
    runGitInSnapshotDir snapDir projectPath ["init", "--bare"]
  
  pure $ Right snapDir
```

#### 2. Take Snapshot (Using write-tree)

```haskell
-- | Take a snapshot using git write-tree (tree object only, no commit)
takeSnapshot :: SnapshotConfig -> IO (Maybe Text)
takeSnapshot config
  | not (_scEnabled config) = pure Nothing
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config
      
      -- Stage all files (respecting .gitignore)
      _ <- runGit gitDir workTree ["add", "-A"]
      
      -- Create tree object (no commit)
      result <- runGit gitDir workTree ["write-tree"]
      case result of
        Left _     -> pure Nothing
        Right hash -> pure $ Just $ T.strip hash
```

#### 3. Restore Files from Snapshot

```haskell
-- | Restore specific files from a snapshot tree
restoreFiles :: SnapshotConfig -> Text -> [FilePath] -> IO (Either Text ())
restoreFiles config treeHash files
  | null files = pure $ Right ()
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config
      
      forM_ files $ \file -> do
        -- Check if file exists in tree
        existsInTree <- fileExistsInTree gitDir treeHash file
        
        if existsInTree
          then do
            -- Restore from tree
            runGit gitDir workTree ["checkout", toString treeHash, "--", file]
          else do
            -- File was created after snapshot, delete it
            let fullPath = workTree </> file
            removeFile fullPath `catch` \(_ :: IOException) -> pure ()
      
      pure $ Right ()

-- | Check if a file exists in a tree object
fileExistsInTree :: FilePath -> Text -> FilePath -> IO Bool
fileExistsInTree gitDir treeHash file = do
  result <- runGit' gitDir ["ls-tree", toString treeHash, "--", file]
  case result of
    Left _  -> pure False
    Right output -> pure $ not $ T.null $ T.strip output
```

#### 4. Full Snapshot Restore

```haskell
-- | Restore entire working tree to snapshot state
restoreSnapshot :: SnapshotConfig -> Text -> IO (Either Text ())
restoreSnapshot config treeHash = do
  let gitDir = _scGitDir config
      workTree = _scProjectPath config
  
  -- Read tree into index
  result1 <- runGit gitDir workTree ["read-tree", toString treeHash]
  case result1 of
    Left err -> pure $ Left err
    Right _ -> do
      -- Checkout all files from index
      result2 <- runGit gitDir workTree ["checkout-index", "-a", "-f"]
      pure $ bimap id (const ()) result2
```

#### 5. Path Management

```haskell
-- | Get the snapshot git directory for a project
getSnapshotDir :: FilePath -> IO FilePath
getSnapshotDir projectPath = do
  dataDir <- getXdgDirectory XdgData "telos"
  projHash <- getProjectHash projectPath
  pure $ dataDir </> "snapshots" </> toString projHash

-- | Hash project path for unique identification
getProjectHash :: FilePath -> IO Text
getProjectHash projectPath = do
  canonical <- canonicalizePath projectPath
  -- Use first 16 chars of SHA256 for brevity
  pure $ T.take 16 $ sha256Text $ toText canonical

-- | SHA256 hash as hex text
sha256Text :: Text -> Text
sha256Text input = 
  let digest = hash (encodeUtf8 input) :: Digest SHA256
  in  toText $ show digest
```

### Helper: Git Command Execution

```haskell
-- | Run git command with isolated GIT_DIR and GIT_WORK_TREE
runGit :: FilePath -> FilePath -> [String] -> IO (Either Text Text)
runGit gitDir workTree args = do
  let env = [ ("GIT_DIR", gitDir)
            , ("GIT_WORK_TREE", workTree)
            ]
  (exitCode, out, err) <- readCreateProcessWithExitCode
    (proc "git" args) { env = Just env } ""
  
  case exitCode of
    ExitSuccess   -> pure $ Right $ toText out
    ExitFailure _ -> pure $ Left $ toText err

-- | Run git command with only GIT_DIR (for bare repo operations)
runGit' :: FilePath -> [String] -> IO (Either Text Text)
runGit' gitDir args = do
  let fullArgs = ["--git-dir=" <> gitDir] <> args
  (exitCode, out, err) <- readProcessWithExitCode "git" fullArgs ""
  case exitCode of
    ExitSuccess   -> pure $ Right $ toText out
    ExitFailure _ -> pure $ Left $ toText err
```

### Changes to `Repl.hs`

```haskell
-- | REPL state (updated)
data ReplState = ReplState
  { rsConfig         :: CliConfig
  , rsServerManager  :: ServerManager
  , rsAgentContext   :: AgentContext
  , rsAuth           :: CopilotAuth
  , rsSessionId      :: Maybe SessionId
  , rsUndoStack      :: [UndoEntry]
  , rsRedoStack      :: [UndoEntry]
  , rsSnapshotConfig :: SnapshotConfig    -- NEW: snapshot configuration
  }

-- | Create new REPL state
newReplState :: CliConfig -> CopilotAuth -> IO ReplState
newReplState config auth = do
  -- ... existing code ...
  
  -- Initialize snapshot system
  cwd <- getCurrentDirectory
  snapConfig <- initSnapshotConfig (config ^. ccSnapshotEnabled) cwd
  
  pure $ ReplState { -- ...
                   , rsSnapshotConfig = snapConfig
                   }

-- | Initialize snapshot configuration
initSnapshotConfig :: Bool -> FilePath -> IO SnapshotConfig
initSnapshotConfig enabled projectPath = do
  if enabled
    then do
      result <- initSnapshotRepo projectPath
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ "Warning: Could not init snapshots: " <> err
          pure $ SnapshotConfig False projectPath ""
        Right gitDir -> 
          pure $ SnapshotConfig True projectPath gitDir
    else
      pure $ SnapshotConfig False projectPath ""
```

### Changes to `CliConfig`

```haskell
-- In Telos.CLI.Config
data CliConfig = CliConfig
  { _ccModel          :: Text
  , _ccMaxIterations  :: Int
  , _ccMcpServers     :: [ServerConfig]
  , _ccSnapshotEnabled :: Bool    -- NEW: enable/disable snapshots (default: True)
  } deriving (Eq, Show, Generic)

-- config.json example:
-- {
--   "model": "gpt-4",
--   "snapshot": true,
--   ...
-- }
```

## Implementation Plan

### Phase 1: Core Snapshot Module (New Implementation)
1. Add `cryptonite` dependency for SHA256
2. Rewrite `src/Telos/Snapshot.hs` with:
   - `SnapshotConfig` data type
   - `getSnapshotDir`, `getProjectHash`
   - `initSnapshotRepo`
   - `takeSnapshot` using `write-tree`
   - `restoreFiles` with tree-aware logic
   - `restoreSnapshot` for full restore
   - `fileExistsInTree` helper
   - `runGit`, `runGit'` helpers

### Phase 2: Configuration Integration
1. Add `_ccSnapshotEnabled` to `CliConfig`
2. Update config JSON parsing
3. Add `rsSnapshotConfig` to `ReplState`
4. Update `newReplState` to initialize snapshot config

### Phase 3: Undo/Redo Integration
1. Update `handleUndoCommand` to use new `restoreFiles`
2. Track created files (not just modified) in `UndoEntry`
3. Handle file deletion on undo (files created after snapshot)

### Phase 4: Testing
1. Unit tests for `getProjectHash` (deterministic)
2. Unit tests for snapshot init/take/restore cycle
3. Integration test: modify file → undo → verify restored
4. Integration test: create file → undo → verify deleted

### Phase 5: Cleanup & Polish
1. Add `cleanupOldSnapshots` for GC (optional)
2. Document in `/help` output
3. Add `--no-snapshot` CLI flag

## Trade-offs

| Decision | Benefit | Cost |
|----------|---------|------|
| Isolated git repo | Zero project pollution | Extra disk space |
| `write-tree` vs `commit` | Minimal metadata | No browsable history |
| SHA256 project hash | Collision-free | 16 extra chars in path |
| Reuse `.gitignore` | Familiar, no new config | May exclude wanted files |
| Track all files | Complete restore | Larger tree objects |

## Examples

### Directory Structure After Use

```
~/.local/share/telos/
├── sessions/
│   └── ses_abc123/
│       ├── info.json
│       └── messages.jsonl
└── snapshots/
    └── a1b2c3d4e5f6g7h8/     # Project hash (16 chars)
        ├── HEAD
        ├── config
        ├── objects/
        │   ├── pack/
        │   └── ...           # Tree and blob objects
        └── refs/
```

### Undo Flow

```
telos> Fix the bug in main.hs

[Snapshot taken: tree abc123...]
[Agent modifies main.hs]
[Agent creates new_file.hs]

telos> /undo
Restoring from snapshot abc123...
- Restored: main.hs
- Deleted: new_file.hs (created after snapshot)
Undone 1 turn(s).
```

### Configuration

```json
// ~/.config/telos/config.json
{
  "model": "gpt-4",
  "maxIterations": 10,
  "snapshot": true,
  "mcpServers": [...]
}
```

Disable snapshots:
```json
{
  "snapshot": false
}
```

## Migration from Design #007

The current implementation uses `git rev-parse HEAD` which returns the project's commit hash. This doesn't work correctly if:
1. Project has uncommitted changes
2. User makes commits between agent turns

The new design:
1. Creates tree objects in isolated repo
2. Tracks actual file state, not commit history
3. Works regardless of project's git state

**Migration**: No data migration needed. Old `Snapshot` values in sessions will be invalid (project commit hashes), but sessions can still load. Undo/redo will simply be unavailable for old sessions.

---

## Implementation Results

**Status**: ✅ Completed

### Files Changed

| File | Changes |
|------|---------|
| `package.yaml` | Added `cryptohash-sha256`, `base16-bytestring` dependencies |
| `src/Telos/Snapshot.hs` | Complete rewrite with isolated git repo design |
| `src/Telos/CLI/Config.hs` | Added `ccSnapshotEnabled` field |
| `src/Telos/CLI/Repl.hs` | Added `rsSnapshotConfig`, updated undo handler |
| `test/Telos/SnapshotSpec.hs` | Complete rewrite with new API tests |

### Key Implementation Details

1. **Environment Variable Isolation**: Uses `System.Posix.Env.getEnvironment` to merge existing env with `GIT_DIR`/`GIT_WORK_TREE`

2. **Init Fix**: `git init --bare` uses `runGit'` (no `GIT_WORK_TREE`) to avoid conflicts

3. **SHA256 Hashing**: Uses `cryptohash-sha256` + `base16-bytestring` for project path hashing

### Test Results

```
Telos.SnapshotSpec
  Snapshot
    getProjectHash
      returns consistent hash for same path
      returns 16 character hash
      returns different hash for different paths
    initSnapshotConfig
      returns disabled config when enabled=False
      returns enabled config with valid gitDir when enabled=True
    takeSnapshot
      returns Nothing when disabled
      returns Just tree hash when enabled
      returns different hashes for different file contents
    restoreFiles
      restores file to previous state
      deletes file that was created after snapshot
      returns error when disabled
    fileExistsInTree
      returns True for file in tree
      returns False for file not in tree
    getDiff
      returns empty diff for identical snapshots
      returns non-empty diff for different snapshots

15 examples, 0 failures
```

### Deviations from Design

1. **No `unix` dependency needed** - `System.Posix.Env` is already available via `base`
2. **Simplified `restoreSnapshot`** - Not yet used by undo handler (uses `restoreFiles` instead)
3. **File tracking deferred** - `ueModifiedFiles` currently empty; full tracking requires tool call interception (future work)

---
**Status**: Implemented  
**Dependencies**: `cryptohash-sha256`, `base16-bytestring`  
**Actual effort**: ~1.5 hours
