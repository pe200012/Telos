module Telos.Tool.Registry
  ( builtinTools
  , getBuiltinTool
  , builtinToolList
  , executeBuiltinTool
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
import           Telos.Tool.Types ( BuiltinTool(..)
                                  , ToolContext
                                  , ToolResult(..)
                                  , btExecutor
                                  , btTool
                                  )
import           Telos.Tool.Write ( writeTool )

builtinTools :: Map.Map Text BuiltinTool
builtinTools
  = Map.fromList
    [ ( "bash", bashTool )
    , ( "read", readTool )
    , ( "write", writeTool )
    , ( "edit", editTool )
    , ( "glob", globTool )
    , ( "grep", grepTool )
    ]

getBuiltinTool :: Text -> Maybe BuiltinTool
getBuiltinTool name = Map.lookup name builtinTools

builtinToolList :: [ Tool ]
builtinToolList = map (^. btTool) (Map.elems builtinTools)

executeBuiltinTool :: ToolContext -> Text -> Value -> IO (Maybe ToolResult)
executeBuiltinTool ctx name args = case getBuiltinTool name of
  Nothing -> pure Nothing
  Just bt -> Just <$> (bt ^. btExecutor) ctx args
