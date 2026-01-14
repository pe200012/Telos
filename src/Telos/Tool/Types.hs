module Telos.Tool.Types
  ( ToolExecutor
  , ToolExecutorType(..)
  , StreamingToolExecutor
  , StreamCallback
  , ToolResult(..)
  , trSuccess
  , trOutput
  , mkToolResult
  , BuiltinTool(..)
  , btTool
  , btExecutor
  , runSimpleExecutor
  , ToolContext(..)
  , tcReadFiles
  , FileReadInfo(..)
  , friReadTime
  , friModTime
  , newToolContext
  , markFileRead
  , wasFileRead
  , getFileReadTime
  , assertFileRead
  , FileAssertError(..)
  ) where

import           Control.Lens     ( makeLenses )

import           Data.Aeson       ( Value )
import qualified Data.Map.Strict  as Map
import           Data.Time        ( UTCTime, getCurrentTime )

import           Relude

import           System.Directory ( getModificationTime )

import           Telos.Core.Types ( Tool )

data ToolResult = ToolResult { _trSuccess :: Bool, _trOutput :: Text }
  deriving stock ( Eq, Show )

makeLenses ''ToolResult

mkToolResult :: Bool -> Text -> ToolResult
mkToolResult = ToolResult

data FileReadInfo = FileReadInfo { _friReadTime :: UTCTime, _friModTime :: Maybe UTCTime }
  deriving stock ( Eq, Show )

makeLenses ''FileReadInfo

newtype ToolContext = ToolContext { _tcReadFiles :: TVar (Map.Map FilePath FileReadInfo) }

makeLenses ''ToolContext

newToolContext :: IO ToolContext
newToolContext = ToolContext <$> newTVarIO Map.empty

markFileRead :: ToolContext -> FilePath -> Maybe UTCTime -> IO ()
markFileRead ctx path mModTime = do
  now <- getCurrentTime
  let info = FileReadInfo now mModTime
  atomically $ modifyTVar' (_tcReadFiles ctx) (Map.insert path info)

wasFileRead :: ToolContext -> FilePath -> IO Bool
wasFileRead ctx path = Map.member path <$> readTVarIO (_tcReadFiles ctx)

getFileReadTime :: ToolContext -> FilePath -> IO (Maybe FileReadInfo)
getFileReadTime ctx path = Map.lookup path <$> readTVarIO (_tcReadFiles ctx)

-- | Error types for file assertion
data FileAssertError
  = FileNotRead FilePath
  | FileModifiedSinceRead FilePath UTCTime UTCTime  -- ^ path, lastMod, lastRead
  deriving stock ( Eq, Show )

-- | Assert that a file was read and hasn't been modified since.
-- Returns Left error if:
--   1. File was never read in this session
--   2. File's mtime is newer than when it was last read
assertFileRead :: ToolContext -> FilePath -> IO (Either FileAssertError ())
assertFileRead ctx path = do
  mInfo <- getFileReadTime ctx path
  case mInfo of
    Nothing   -> pure $ Left $ FileNotRead path
    Just info -> do
      -- Check if file was modified since last read
      currentMtime <- getModificationTime path
      let readTime = _friReadTime info
      if currentMtime > readTime
        then pure $ Left $ FileModifiedSinceRead path currentMtime readTime
        else pure $ Right ()

-- | Callback for streaming output chunks
type StreamCallback = Text -> IO ()

-- | Simple tool executor that only needs ToolContext
type ToolExecutor = ToolContext -> Value -> IO ToolResult

-- | Streaming tool executor that receives a callback for incremental output
type StreamingToolExecutor = StreamCallback -> ToolContext -> Value -> IO ToolResult

-- | Executor type that supports both simple and agent-aware tools
-- AgentExecutor will be used by tools like 'task' that need to spawn subagents
data ToolExecutorType
  = SimpleExecutor ToolExecutor
  | StreamingExecutor StreamingToolExecutor
  | AgentExecutor  -- Placeholder: actual implementation requires AgentContext
  deriving stock ( Generic )

data BuiltinTool = BuiltinTool { _btTool :: Tool, _btExecutor :: ToolExecutorType }

makeLenses ''BuiltinTool

-- | Extract the executor function from a SimpleExecutor, or return Nothing for AgentExecutor
runSimpleExecutor :: BuiltinTool -> Maybe ToolExecutor
runSimpleExecutor bt = case _btExecutor bt of
  SimpleExecutor exec -> Just exec
  _ -> Nothing
