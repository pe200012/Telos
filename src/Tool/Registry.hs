{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Tool.Registry
  ( Tool
  , ToolResult
  , HasToolResultId(..)
  , HasToolResultName(..)
  , HasToolResultContent(..)
  , toolSpecs
  , toolInventoryMessage
  , findTool
  , mkToolResult
  ) where

import           Control.Lens    ( (.~) )
import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Aeson      ( (.=), Value, object )
import qualified Data.Text       as Text

import           Relude

import           Tool.Schema     ( ToolSpec
                                 , defaultToolFunctionSpec
                                 , defaultToolSpec
                                 , toolDescription
                                 , toolFunction
                                 , toolName
                                 , toolParameters
                                 , toolType
                                 )

import           Types.Chat      ( Message, Role(System), mkMessage )

data Tool = Tool { _toolName :: Text, _toolDescription :: Text, _toolParameters :: Value }
  deriving ( Eq, Show, Generic )

data ToolResult
  = ToolResult { _toolResultId :: Text, _toolResultName :: Text, _toolResultContent :: Text }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ToolResult

toolSpecs :: [ ToolSpec ]
toolSpecs = map toSpec toolDefinitions
  where
    toSpec tool
      = defaultToolSpec
      & toolType .~ "function"
      & toolFunction
      .~ (defaultToolFunctionSpec
          & toolName .~ _toolName tool
          & toolDescription .~ _toolDescription tool
          & toolParameters .~ _toolParameters tool)

toolInventoryMessage :: Message
toolInventoryMessage = mkMessage System (renderInventory toolDefinitions)

findTool :: Text -> Maybe Tool
findTool name = find (\tool -> _toolName tool == name) toolDefinitions

mkToolResult :: Text -> Text -> Text -> ToolResult
mkToolResult callId name content
  = ToolResult { _toolResultId = callId, _toolResultName = name, _toolResultContent = content }

toolDefinitions :: [ Tool ]
toolDefinitions
  = [ Tool
      { _toolName        = "list_files"
      , _toolDescription = "List files under a path"
      , _toolParameters  = object
          [ "type" .= ("object" :: Text)
          , "properties"
            .= object
              [ "path"
                .= object
                  [ "type" .= ("string" :: Text), "description" .= ("Path or directory" :: Text) ]
              ]
          , "required" .= [ "path" :: Text ]
          ]
      }
    , Tool
      { _toolName        = "read_file"
      , _toolDescription = "Read a UTF-8 text file"
      , _toolParameters  = object
          [ "type" .= ("object" :: Text)
          , "properties"
            .= object
              [ "path"
                .= object [ "type" .= ("string" :: Text), "description" .= ("File path" :: Text) ]
              ]
          , "required" .= [ "path" :: Text ]
          ]
      }
    , Tool
      { _toolName        = "write_file"
      , _toolDescription = "Write content to a file"
      , _toolParameters  = object
          [ "type" .= ("object" :: Text)
          , "properties"
            .= object
              [ "path"
                .= object [ "type" .= ("string" :: Text), "description" .= ("File path" :: Text) ]
              , "content"
                .= object
                  [ "type" .= ("string" :: Text), "description" .= ("File content" :: Text) ]
              ]
          , "required" .= [ "path" :: Text, "content" :: Text ]
          ]
      }
    , Tool
      { _toolName        = "grep"
      , _toolDescription = "Search for a substring in files under a path"
      , _toolParameters  = object
          [ "type" .= ("object" :: Text)
          , "properties"
            .= object
              [ "path"
                .= object
                  [ "type" .= ("string" :: Text), "description" .= ("Path or directory" :: Text) ]
              , "pattern"
                .= object
                  [ "type" .= ("string" :: Text), "description" .= ("Substring to match" :: Text) ]
              ]
          , "required" .= [ "path" :: Text, "pattern" :: Text ]
          ]
      }
    , Tool { _toolName        = "bash"
           , _toolDescription = "Run an allowed shell command"
           , _toolParameters  = object
               [ "type" .= ("object" :: Text)
               , "properties"
                 .= object
                   [ "command"
                     .= object
                       [ "type" .= ("string" :: Text), "description" .= ("Command name" :: Text) ]
                   , "args"
                     .= object
                       [ "type" .= ("array" :: Text)
                       , "items" .= object [ "type" .= ("string" :: Text) ]
                       , "description" .= ("Command arguments" :: Text)
                       ]
                   ]
               , "required" .= [ "command" :: Text ]
               ]
           }
    , Tool { _toolName        = "apply_patch"
           , _toolDescription = "Apply a unified diff patch"
           , _toolParameters  = object
               [ "type" .= ("object" :: Text)
               , "properties"
                 .= object
                   [ "patch"
                     .= object
                       [ "type" .= ("string" :: Text)
                       , "description" .= ("Unified diff patch text" :: Text)
                       ]
                   ]
               , "required" .= [ "patch" :: Text ]
               ]
           }
    ]

renderInventory :: [ Tool ] -> Text
renderInventory tools
  = let
      renderTool tool
        = Text.unlines
          [ "- " <> _toolName tool <> ": " <> _toolDescription tool
          , "  params: " <> Text.pack (show (_toolParameters tool))
          ]
    in 
      Text.unlines
        ([ "You can call tools using <toolcall>{...}</toolcall>." ] <> map renderTool tools)
