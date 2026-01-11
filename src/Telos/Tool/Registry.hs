module Telos.Tool.Registry
  ( builtinTools
  , getBuiltinTool
  , builtinToolList
  , executeBuiltinTool
  ) where

import qualified Data.Map.Strict  as Map

import           Telos.Core.Types ( Tool )
import           Telos.Tool.Types ( BuiltinTool(..), ToolResult(..), ToolContext, btTool, btExecutor )
import           Telos.Tool.Bash  ( bashTool )
import           Telos.Tool.Read  ( readTool )
import           Telos.Tool.Write ( writeTool )
import           Telos.Tool.Edit  ( editTool )
import           Telos.Tool.Glob  ( globTool )
import           Telos.Tool.Grep  ( grepTool )

import           Data.Aeson       ( Value )
import           Lens.Micro       ( (^.) )

builtinTools :: Map.Map Text BuiltinTool
builtinTools = Map.fromList
  [ ("bash", bashTool)
  , ("read", readTool)
  , ("write", writeTool)
  , ("edit", editTool)
  , ("glob", globTool)
  , ("grep", grepTool)
  ]

getBuiltinTool :: Text -> Maybe BuiltinTool
getBuiltinTool name = Map.lookup name builtinTools

builtinToolList :: [Tool]
builtinToolList = map (^. btTool) (Map.elems builtinTools)

executeBuiltinTool :: ToolContext -> Text -> Value -> IO (Maybe ToolResult)
executeBuiltinTool ctx name args = case getBuiltinTool name of
  Nothing -> pure Nothing
  Just bt -> Just <$> (bt ^. btExecutor) ctx args
