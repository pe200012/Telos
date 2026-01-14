# Design Log #004: Persistent Sessions

## Background

Telos CLI currently keeps conversation history in memory (`TVar [Message]`). Users lose context when restarting. Need persistence like OpenCode.

## Design Decisions

| Question | Decision |
|----------|----------|
| Storage backend | File system JSON |
| Session granularity | Separated (info.json + messages.jsonl) |
| Recovery timing | Default new session + `/load` command |
| Storage location | XDG standard (`~/.local/share/telos/`) |

## Directory Structure

```
~/.local/share/telos/
└── sessions/
    ├── {sessionId}/
    │   ├── info.json      # SessionInfo metadata
    │   └── messages.jsonl # Message stream (one per line)
    └── ...
```

## New Modules

### 1. Telos.Storage.Types

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.Storage.Types
  ( SessionId(..)
  , SessionInfo(..)
  , siId, siTitle, siCreatedAt, siUpdatedAt
  , StoredMessage(..)
  , smIndex, smMessage, smTimestamp
  , generateSessionId
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

import Telos.Core.Types (Message)

-- | Unique session identifier
newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON SessionId
instance ToJSON SessionId

-- | Session metadata (stored in info.json)
data SessionInfo = SessionInfo
  { _siId        :: SessionId
  , _siTitle     :: Text
  , _siCreatedAt :: UTCTime
  , _siUpdatedAt :: UTCTime
  } deriving (Eq, Show, Generic)

makeLenses ''SessionInfo
instance FromJSON SessionInfo
instance ToJSON SessionInfo

-- | Single message with metadata (one line in messages.jsonl)
data StoredMessage = StoredMessage
  { _smIndex     :: Int      -- Message index in session
  , _smMessage   :: Message  -- The actual message
  , _smTimestamp :: UTCTime  -- When it was added
  } deriving (Eq, Show, Generic)

makeLenses ''StoredMessage
instance FromJSON StoredMessage
instance ToJSON StoredMessage

-- | Generate a new unique session ID (timestamp-based for sorting)
generateSessionId :: IO SessionId
generateSessionId = do
  uuid <- UUID.nextRandom
  pure $ SessionId $ "ses_" <> T.pack (show uuid)
```

### 2. Telos.Storage.Path

```haskell
module Telos.Storage.Path
  ( getDataDir
  , getSessionsDir
  , getSessionDir
  , getSessionInfoPath
  , getMessagesPath
  , ensureSessionDir
  ) where

import System.Directory (XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))

import Telos.Storage.Types (SessionId(..))

-- | Get XDG data directory: ~/.local/share/telos
getDataDir :: IO FilePath
getDataDir = getXdgDirectory XdgData "telos"

-- | Get sessions directory: ~/.local/share/telos/sessions
getSessionsDir :: IO FilePath
getSessionsDir = do
  dataDir <- getDataDir
  pure $ dataDir </> "sessions"

-- | Get specific session directory
getSessionDir :: SessionId -> IO FilePath
getSessionDir (SessionId sid) = do
  sessionsDir <- getSessionsDir
  pure $ sessionsDir </> T.unpack sid

-- | Get path to session info.json
getSessionInfoPath :: SessionId -> IO FilePath
getSessionInfoPath sid = do
  sessionDir <- getSessionDir sid
  pure $ sessionDir </> "info.json"

-- | Get path to messages.jsonl
getMessagesPath :: SessionId -> IO FilePath
getMessagesPath sid = do
  sessionDir <- getSessionDir sid
  pure $ sessionDir </> "messages.jsonl"

-- | Ensure session directory exists
ensureSessionDir :: SessionId -> IO FilePath
ensureSessionDir sid = do
  sessionDir <- getSessionDir sid
  createDirectoryIfMissing True sessionDir
  pure sessionDir
```

### 3. Telos.Storage.Session

```haskell
module Telos.Storage.Session
  ( -- * Session lifecycle
    createSession
  , getSession
  , listSessions
  , deleteSession
  , touchSession
    -- * Message operations
  , appendMessage
  , loadMessages
  , getMessageCount
    -- * Context integration
  , saveContext
  , loadContext
  ) where

import Control.Exception (catch, IOException)
import Control.Monad (forM, filterM)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (listDirectory, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(..), hPutStrLn)

import Telos.Agent.Context (AgentContext, newAgentContext, getHistory, setHistory)
import Telos.Agent.Config (AgentConfig)
import Telos.Core.Types (Message)
import Telos.Storage.Types
import Telos.Storage.Path

-- | Create a new session
createSession :: Maybe Text -> IO SessionInfo
createSession mTitle = do
  sid <- generateSessionId
  now <- getCurrentTime
  let title = fromMaybe "Untitled Session" mTitle
      info = SessionInfo sid title now now
  
  _ <- ensureSessionDir sid
  infoPath <- getSessionInfoPath sid
  BL.writeFile infoPath (encode info)
  
  -- Create empty messages file
  msgsPath <- getMessagesPath sid
  writeFile msgsPath ""
  
  pure info

-- | Get session by ID
getSession :: SessionId -> IO (Maybe SessionInfo)
getSession sid = do
  infoPath <- getSessionInfoPath sid
  catch (Just <$> readSessionInfo infoPath) handleNotFound
  where
    readSessionInfo path = do
      content <- BL.readFile path
      case eitherDecode content of
        Left err -> error $ "Failed to parse session info: " <> err
        Right info -> pure info
    handleNotFound :: IOException -> IO (Maybe SessionInfo)
    handleNotFound _ = pure Nothing

-- | List all sessions (sorted by updated time, newest first)
listSessions :: IO [SessionInfo]
listSessions = do
  sessionsDir <- getSessionsDir
  exists <- doesDirectoryExist sessionsDir
  if not exists
    then pure []
    else do
      entries <- listDirectory sessionsDir
      sessions <- forM entries $ \entry -> do
        let sid = SessionId (T.pack entry)
        getSession sid
      pure $ sortBy (comparing (Down . _siUpdatedAt)) (catMaybes sessions)

-- | Delete a session and all its messages
deleteSession :: SessionId -> IO ()
deleteSession sid = do
  sessionDir <- getSessionDir sid
  exists <- doesDirectoryExist sessionDir
  when exists $ removeDirectoryRecursive sessionDir

-- | Update session's updatedAt timestamp
touchSession :: SessionId -> IO ()
touchSession sid = do
  mInfo <- getSession sid
  case mInfo of
    Nothing -> pure ()
    Just info -> do
      now <- getCurrentTime
      let updated = info { _siUpdatedAt = now }
      infoPath <- getSessionInfoPath sid
      BL.writeFile infoPath (encode updated)

-- | Append a message to session (JSONL format)
appendMessage :: SessionId -> Message -> IO ()
appendMessage sid msg = do
  now <- getCurrentTime
  msgsPath <- getMessagesPath sid
  
  -- Get current message count for index
  count <- getMessageCount sid
  let stored = StoredMessage count msg now
  
  -- Append as JSONL
  withFile msgsPath AppendMode $ \h ->
    BLC.hPutStrLn h (encode stored)
  
  -- Update session timestamp
  touchSession sid

-- | Load all messages from session
loadMessages :: SessionId -> IO [Message]
loadMessages sid = do
  msgsPath <- getMessagesPath sid
  catch (parseMessages msgsPath) handleNotFound
  where
    parseMessages path = do
      content <- BL.readFile path
      let lines' = BLC.lines content
      pure $ mapMaybe parseLine lines'
    
    parseLine line
      | BL.null line = Nothing
      | otherwise = case eitherDecode line of
          Left _ -> Nothing
          Right stored -> Just (_smMessage stored)
    
    handleNotFound :: IOException -> IO [Message]
    handleNotFound _ = pure []

-- | Get number of messages in session
getMessageCount :: SessionId -> IO Int
getMessageCount sid = do
  msgs <- loadMessages sid
  pure $ length msgs

-- | Save AgentContext history to session
saveContext :: SessionId -> AgentContext -> IO ()
saveContext sid ctx = do
  history <- getHistory ctx
  existingCount <- getMessageCount sid
  
  -- Only append new messages
  let newMessages = drop existingCount history
  forM_ newMessages $ \msg ->
    appendMessage sid msg

-- | Load session into a new AgentContext
loadContext :: SessionId -> AgentConfig -> IO (Maybe (SessionInfo, AgentContext))
loadContext sid config = do
  mInfo <- getSession sid
  case mInfo of
    Nothing -> pure Nothing
    Just info -> do
      messages <- loadMessages sid
      ctx <- newAgentContext config
      setHistory ctx messages
      pure $ Just (info, ctx)
```

## REPL Commands

### New Commands

| Command | Description |
|---------|-------------|
| `/sessions` | List all saved sessions |
| `/load <id>` | Load session by ID (prefix match supported) |
| `/save [title]` | Save current session (creates new if none) |
| `/new` | Start new session (saves current first if dirty) |

### Implementation in Repl.hs

```haskell
-- Add to ReplState
data ReplState = ReplState
  { _rsContext :: AgentContext
  , _rsConfig :: AgentConfig
  , _rsSessionId :: Maybe SessionId  -- NEW: current session
  , _rsDirty :: Bool                 -- NEW: has unsaved changes
  , ...
  }

-- Command handlers
handleSessions :: ReplState -> IO ()
handleSessions _ = do
  sessions <- listSessions
  if null sessions
    then putStrLn "No saved sessions."
    else do
      putStrLn "Sessions:"
      forM_ sessions $ \s -> do
        let sid = unSessionId (_siId s)
            title = _siTitle s
            updated = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (_siUpdatedAt s)
        putStrLn $ "  " <> T.unpack sid <> " | " <> T.unpack title <> " | " <> updated

handleLoad :: ReplState -> Text -> IO ReplState
handleLoad state prefix = do
  sessions <- listSessions
  let matches = filter (T.isPrefixOf prefix . unSessionId . _siId) sessions
  case matches of
    [] -> do
      putStrLn $ "No session found matching: " <> T.unpack prefix
      pure state
    [s] -> do
      mLoaded <- loadContext (_siId s) (_rsConfig state)
      case mLoaded of
        Nothing -> do
          putStrLn "Failed to load session."
          pure state
        Just (info, ctx) -> do
          putStrLn $ "Loaded session: " <> T.unpack (_siTitle info)
          pure state { _rsContext = ctx, _rsSessionId = Just (_siId info), _rsDirty = False }
    _ -> do
      putStrLn "Multiple sessions match. Be more specific:"
      forM_ matches $ \s ->
        putStrLn $ "  " <> T.unpack (unSessionId (_siId s))
      pure state

handleSave :: ReplState -> Maybe Text -> IO ReplState
handleSave state mTitle = do
  case _rsSessionId state of
    Just sid -> do
      saveContext sid (_rsContext state)
      putStrLn "Session saved."
      pure state { _rsDirty = False }
    Nothing -> do
      info <- createSession mTitle
      saveContext (_siId info) (_rsContext state)
      putStrLn $ "Created and saved session: " <> T.unpack (unSessionId (_siId info))
      pure state { _rsSessionId = Just (_siId info), _rsDirty = False }

handleNew :: ReplState -> IO ReplState
handleNew state = do
  -- Save current if dirty
  when (_rsDirty state) $ do
    putStrLn "Saving current session..."
    void $ handleSave state Nothing
  
  -- Create fresh context
  ctx <- newAgentContext (_rsConfig state)
  putStrLn "Started new session."
  pure state { _rsContext = ctx, _rsSessionId = Nothing, _rsDirty = False }
```

## Auto-save Integration

Option 1: Save after each assistant response (recommended)
Option 2: Save on `/quit` only
Option 3: Periodic auto-save (every N messages)

Recommend Option 1 for data safety.

## Implementation Plan

### Phase 1: Core Storage
1. Create `src/Telos/Storage/Types.hs`
2. Create `src/Telos/Storage/Path.hs`
3. Create `src/Telos/Storage/Session.hs`
4. Ensure `Message` has proper `ToJSON`/`FromJSON` (verify existing)

### Phase 2: CLI Integration
1. Add `ReplState` fields for session tracking
2. Implement `/sessions`, `/load`, `/save`, `/new` commands
3. Add auto-save after assistant responses
4. Update `/quit` to prompt save if dirty

### Phase 3: Testing
1. Unit tests for Storage.Session
2. Integration test: create → save → load cycle
3. Test JSONL append correctness

## Trade-offs

| Choice | Benefit | Cost |
|--------|---------|------|
| JSONL vs JSON array | Append-only, crash-safe | Parse all lines on load |
| XDG vs project-local | Standard, global sessions | No project isolation |
| UUID vs timestamp ID | Collision-free | Less readable |

## Examples

### Session Workflow
```
$ telos-exe
> Hello, how are you?
[Assistant responds]

> /save "My coding session"
Created and saved session: ses_abc123...

> /quit

$ telos-exe
> /sessions
Sessions:
  ses_abc123... | My coding session | 2026-01-11 01:50

> /load ses_abc
Loaded session: My coding session

> [Continue conversation...]
```

### File Contents

**info.json**:
```json
{
  "_siId": "ses_abc123-def456",
  "_siTitle": "My coding session",
  "_siCreatedAt": "2026-01-11T01:50:00Z",
  "_siUpdatedAt": "2026-01-11T02:15:00Z"
}
```

**messages.jsonl**:
```json
{"_smIndex":0,"_smMessage":{"tag":"UserMessage","_umContent":"Hello, how are you?"},"_smTimestamp":"2026-01-11T01:50:00Z"}
{"_smIndex":1,"_smMessage":{"tag":"AssistantMsg","contents":{...}},"_smTimestamp":"2026-01-11T01:50:05Z"}
```
