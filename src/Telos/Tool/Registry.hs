module Telos.Tool.Registry
  ( builtinTools
  , getBuiltinTool
  , builtinToolList
  , executeBuiltinTool
  , isAgentTool
  , isStreamingTool
  , taskToolName
  ) where

import           Data.Aeson       ( Value )
import qualified Data.Map.Strict  as Map

import           Lens.Micro       ( (^.) )

import           Relude

import           Telos.Core.Types ( Tool )
import           Telos.Tool.Bash  ( bashTool )
import           Telos.Tool.Edit  ( editTool )
import           Telos.Tool.Glob  ( globTool )
import           Telos.Tool.Grep  ( grepTool )
import           Telos.Tool.Read  ( readTool )
import           Telos.Tool.Task  ( taskTool )
import           Telos.Tool.Types ( BuiltinTool(..)
                                  , ToolExecutorType(..)
                                  , ToolContext
                                  , ToolResult(..)
                                  , StreamCallback
                                  , btExecutor
                                  , btTool
                                  )
import           Telos.Tool.Write ( writeTool )

-- | Name of the task tool (for special handling in Loop)
taskToolName :: Text
taskToolName = "task"

builtinTools :: Map.Map Text BuiltinTool
builtinTools
  = Map.fromList
    [ ( "bash", bashTool )
    , ( "read", readTool )
    , ( "write", writeTool )
    , ( "edit", editTool )
    , ( "glob", globTool )
    , ( "grep", grepTool )
    , ( taskToolName, BuiltinTool taskTool AgentExecutor )
    ]

getBuiltinTool :: Text -> Maybe BuiltinTool
getBuiltinTool name = Map.lookup name builtinTools

-- | Check if a tool name is an agent-aware tool (needs AgentContext)
isAgentTool :: Text -> Bool
isAgentTool name = case getBuiltinTool name of
  Just bt -> case bt ^. btExecutor of
    AgentExecutor -> True
    _             -> False
  Nothing -> False

builtinToolList :: [ Tool ]
builtinToolList = map (^. btTool) (Map.elems builtinTools)

-- | Check if a tool name is a streaming tool
isStreamingTool :: Text -> Bool
isStreamingTool name = case getBuiltinTool name of
  Just bt -> case bt ^. btExecutor of
    StreamingExecutor _ -> True
    _                   -> False
  Nothing -> False

-- | Execute a builtin tool with optional streaming callback
executeBuiltinTool :: StreamCallback -> ToolContext -> Text -> Value -> IO (Maybe ToolResult)
executeBuiltinTool onChunk ctx name args = case getBuiltinTool name of
  Nothing -> pure Nothing
  Just bt -> case bt ^. btExecutor of
    SimpleExecutor exec -> Just <$> exec ctx args
    StreamingExecutor exec -> Just <$> exec onChunk ctx args
    AgentExecutor -> pure $ Just $ ToolResult False "Agent executor not available in this context"
