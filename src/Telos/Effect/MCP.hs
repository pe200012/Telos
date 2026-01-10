module Telos.Effect.MCP
  ( MCP(..)
  , listTools
  , callTool
  , listResources
  , readResource
  , ToolResult(..)
  , ContentItem(..)
  , Resource(..)
  , ResourceContent(..)
  ) where

import           Data.Aeson       ( Value )
import           Data.Text        ( Text )

import           GHC.Generics     ( Generic )

import           Polysemy         ( Sem, makeSem )

import           Telos.Core.Error ( MCPError )
import           Telos.Core.Types ( Tool )

data ContentItem
  = TextContent Text
  | ImageContent { icMimeType :: Text, icBase64Data :: Text }
  | EmbeddedResource { erUri :: Text, erText :: Text }
  deriving stock ( Eq, Show, Generic )

data ToolResult = ToolResult { trContent :: [ ContentItem ], trIsError :: Bool }
  deriving stock ( Eq, Show, Generic )

data Resource
  = Resource
  { resUri :: Text, resName :: Text, resMimeType :: Maybe Text, resDescription :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

data ResourceContent
  = ResourceContent
  { rcUri :: Text, rcMimeType :: Maybe Text, rcText :: Maybe Text, rcBlob :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

data MCP m a where
  ListTools :: MCP m [ Tool ]
  CallTool :: Text -> Value -> MCP m (Either MCPError ToolResult)
  ListResources :: MCP m [ Resource ]
  ReadResource :: Text -> MCP m (Either MCPError ResourceContent)

makeSem ''MCP
