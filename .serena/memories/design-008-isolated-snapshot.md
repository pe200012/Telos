# Design #008: Isolated Snapshot Storage

## Summary
Rewrote snapshot system to use isolated Git repository instead of project's `.git`.

## Key Files
- `src/Telos/Snapshot.hs` - Complete rewrite
- `src/Telos/CLI/Config.hs` - Added `ccSnapshotEnabled`
- `src/Telos/CLI/Repl.hs` - Added `rsSnapshotConfig`, `UndoEntry.ueModifiedFiles`
- `test/Telos/SnapshotSpec.hs` - 15 tests

## Architecture
- Snapshot repo: `~/.local/share/telos/snapshots/{project-hash}/`
- Uses `git write-tree` (tree objects only, no commits)
- `GIT_DIR` + `GIT_WORK_TREE` env vars for isolation
- SHA256 hash of canonical project path for unique identification

## Key Types
```haskell
data SnapshotConfig = SnapshotConfig
  { _scEnabled     :: Bool
  , _scProjectPath :: FilePath
  , _scGitDir      :: FilePath
  }

-- Functions
initSnapshotConfig :: Bool -> FilePath -> IO SnapshotConfig
takeSnapshot :: SnapshotConfig -> IO (Maybe Text)  -- Returns tree hash
restoreFiles :: SnapshotConfig -> Text -> [FilePath] -> IO (Either Text ())
fileExistsInTree :: FilePath -> Text -> FilePath -> IO Bool
getDiff :: SnapshotConfig -> Text -> Text -> IO Text
```

## Dependencies Added
- `cryptohash-sha256`
- `base16-bytestring`

## Status
✅ Implemented, 15/15 tests passing
