{-# LANGUAGE TemplateHaskell #-}

module Telos.Effect.MCP
  ( MCP(..)
  , listTools
  , callTool
  , listResources
  , readResource
  , ToolResult
  , makeToolResult
  , trContent
  , trIsError
  , ContentItem(..)
  , icMimeType
  , icBase64Data
  , erUri
  , erText
  , Resource
  , makeResource
  , resUri
  , resName
  , resMimeType
  , resDescription
  ) where

import           Data.Aeson           ( Value )

import           Lens.Micro.TH        ( makeLenses )

import           Polysemy             ( makeSem )

import           Telos.Core.Error     ( MCPError )
import           Telos.Core.Types     ( Tool )
import           Telos.MCP.Types      ( ResourceContents )

data ContentItem
  = TextContent Text
  | ImageContent { _icMimeType :: Text, _icBase64Data :: Text }
  | EmbeddedResource { _erUri :: Text, _erText :: Text }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ContentItem

data ToolResult = ToolResult { _trContent :: [ ContentItem ], _trIsError :: Bool }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ToolResult

makeToolResult :: [ ContentItem ] -> ToolResult
makeToolResult content = ToolResult { _trContent = content, _trIsError = False }

data Resource
  = Resource
  { _resUri :: Text, _resName :: Text, _resMimeType :: Maybe Text, _resDescription :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

makeLenses ''Resource

makeResource :: Text -> Text -> Resource
makeResource uri name
  = Resource { _resUri = uri, _resName = name, _resMimeType = Nothing, _resDescription = Nothing }

data MCP m a where
  ListTools :: MCP m [ Tool ]
  CallTool :: Text -> Value -> MCP m (Either MCPError ToolResult)
  ListResources :: MCP m [ Resource ]
  ReadResource :: Text -> MCP m (Either MCPError ResourceContents)

makeSem ''MCP
