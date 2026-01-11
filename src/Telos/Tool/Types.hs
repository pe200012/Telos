module Telos.Tool.Types
  ( ToolExecutor
  , ToolExecutorType(..)
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
  ) where

import           Data.Aeson       ( Value )
import qualified Data.Map.Strict  as Map
import           Data.Time        ( UTCTime, getCurrentTime )

import           Lens.Micro.TH    ( makeLenses )

import           Relude

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

-- | Simple tool executor that only needs ToolContext
type ToolExecutor = ToolContext -> Value -> IO ToolResult

-- | Executor type that supports both simple and agent-aware tools
-- AgentExecutor will be used by tools like 'task' that need to spawn subagents
data ToolExecutorType
  = SimpleExecutor ToolExecutor
  | AgentExecutor  -- Placeholder: actual implementation requires AgentContext
  deriving stock ( Generic )

data BuiltinTool = BuiltinTool { _btTool :: Tool, _btExecutor :: ToolExecutorType }

makeLenses ''BuiltinTool

-- | Extract the executor function from a SimpleExecutor, or return Nothing for AgentExecutor
runSimpleExecutor :: BuiltinTool -> Maybe ToolExecutor
runSimpleExecutor bt = case _btExecutor bt of
  SimpleExecutor exec -> Just exec
  AgentExecutor       -> Nothing
