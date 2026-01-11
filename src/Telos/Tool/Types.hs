module Telos.Tool.Types
  ( ToolExecutor
  , ToolResult(..)
  , trSuccess
  , trOutput
  , BuiltinTool(..)
  , btTool
  , btExecutor
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

type ToolExecutor = ToolContext -> Value -> IO ToolResult

data BuiltinTool = BuiltinTool { _btTool :: Tool, _btExecutor :: ToolExecutor }

makeLenses ''BuiltinTool
