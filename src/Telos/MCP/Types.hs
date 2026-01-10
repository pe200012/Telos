module Telos.MCP.Types
  ( ServerConfig(..)
  , ServerCapabilities(..)
  , ClientCapabilities(..)
  , ClientInfo(..)
  , ServerInfo(..)
  , InitializeParams(..)
  , InitializeResult(..)
  , CallToolParams(..)
  , CallToolResult(..)
  , ListToolsResult(..)
  , ToolInfo(..)
  , ListResourcesResult(..)
  , ResourceInfo(..)
  , ReadResourceParams(..)
  , ReadResourceResult(..)
  , ResourceContents(..)
  , ContentPart(..)
  , ProtocolVersion
  , currentProtocolVersion
  , RootsCapability(..)
  , SamplingCapability(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types ( Parser )

type ProtocolVersion = Text

currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = "2024-11-05"

data ServerConfig
  = ServerConfig { scName    :: Text
                 , scCommand :: FilePath
                 , scArgs    :: [ String ]
                 , scWorkDir :: Maybe FilePath
                 , scEnv     :: Maybe [ ( String, String ) ]
                 }
  deriving stock ( Eq, Show, Generic )

data ClientInfo = ClientInfo { ciName :: Text, ciVersion :: Text }
  deriving stock ( Eq, Show, Generic )

instance ToJSON ClientInfo where
  toJSON ci = object [ "name" .= ciName ci, "version" .= ciVersion ci ]

data ServerInfo = ServerInfo { siName :: Text, siVersion :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo <$> o .: "name" <*> o .:? "version"

data ClientCapabilities
  = ClientCapabilities { ccRoots :: Maybe RootsCapability, ccSampling :: Maybe SamplingCapability }
  deriving stock ( Eq, Show, Generic )

newtype RootsCapability = RootsCapability { rcListChanged :: Maybe Bool }
  deriving stock ( Eq, Show, Generic )

data SamplingCapability = SamplingCapability
  deriving stock ( Eq, Show, Generic )

instance ToJSON ClientCapabilities where
  toJSON cc
    = object
    $ maybe [] (\r -> [ "roots" .= r ]) (ccRoots cc)
    ++ maybe [] (\s -> [ "sampling" .= s ]) (ccSampling cc)

instance ToJSON RootsCapability where
  toJSON rc = object $ maybe [] (\b -> [ "listChanged" .= b ]) (rcListChanged rc)

instance ToJSON SamplingCapability where
  toJSON _ = object []

data ServerCapabilities
  = ServerCapabilities { scapLogging   :: Maybe Bool
                       , scapPrompts   :: Maybe PromptsCapability
                       , scapResources :: Maybe ResourcesCapability
                       , scapTools     :: Maybe ToolsCapability
                       }
  deriving stock ( Eq, Show, Generic )

newtype PromptsCapability = PromptsCapability { pcListChanged :: Maybe Bool }
  deriving stock ( Eq, Show, Generic )

data ResourcesCapability
  = ResourcesCapability { rscSubscribe :: Maybe Bool, rscListChanged :: Maybe Bool }
  deriving stock ( Eq, Show, Generic )

newtype ToolsCapability = ToolsCapability { toolsListChanged :: Maybe Bool }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \o -> ServerCapabilities <$> o .:? "logging"
    <*> o .:? "prompts"
    <*> o .:? "resources"
    <*> o .:? "tools"

instance FromJSON PromptsCapability where
  parseJSON = withObject "PromptsCapability" $ \o -> PromptsCapability <$> o .:? "listChanged"

instance FromJSON ResourcesCapability where
  parseJSON = withObject "ResourcesCapability" $ \o
    -> ResourcesCapability <$> o .:? "subscribe" <*> o .:? "listChanged"

instance FromJSON ToolsCapability where
  parseJSON = withObject "ToolsCapability" $ \o -> ToolsCapability <$> o .:? "listChanged"

data InitializeParams
  = InitializeParams { ipProtocolVersion :: ProtocolVersion
                     , ipCapabilities    :: ClientCapabilities
                     , ipClientInfo      :: ClientInfo
                     }
  deriving stock ( Eq, Show, Generic )

instance ToJSON InitializeParams where
  toJSON ip
    = object
      [ "protocolVersion" .= ipProtocolVersion ip
      , "capabilities" .= ipCapabilities ip
      , "clientInfo" .= ipClientInfo ip
      ]

data InitializeResult
  = InitializeResult { irProtocolVersion :: ProtocolVersion
                     , irCapabilities    :: ServerCapabilities
                     , irServerInfo      :: Maybe ServerInfo
                     , irInstructions    :: Maybe Text
                     }
  deriving stock ( Eq, Show, Generic )

instance FromJSON InitializeResult where
  parseJSON = withObject "InitializeResult" $ \o -> InitializeResult <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .:? "serverInfo"
    <*> o .:? "instructions"

data ToolInfo = ToolInfo { tiName :: Text, tiDescription :: Maybe Text, tiInputSchema :: Value }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ToolInfo where
  parseJSON = withObject "ToolInfo" $ \o
    -> ToolInfo <$> o .: "name" <*> o .:? "description" <*> o .: "inputSchema"

data ListToolsResult = ListToolsResult { ltrTools :: [ ToolInfo ], ltrNextCursor :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ListToolsResult where
  parseJSON
    = withObject "ListToolsResult" $ \o -> ListToolsResult <$> o .: "tools" <*> o .:? "nextCursor"

data CallToolParams = CallToolParams { ctpName :: Text, ctpArguments :: Maybe Value }
  deriving stock ( Eq, Show, Generic )

instance ToJSON CallToolParams where
  toJSON ctp
    = object $ ("name" .= ctpName ctp) : maybe [] (\a -> [ "arguments" .= a ]) (ctpArguments ctp)

data ContentPart
  = TextPart Text
  | ImagePart { ipData :: Text, ipMimeType :: Text }
  | ResourcePart { rpUri :: Text, rpMimeType :: Maybe Text, rpText :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ContentPart where
  parseJSON = withObject "ContentPart" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text"     -> TextPart <$> o .: "text"
      "image"    -> ImagePart <$> o .: "data" <*> o .: "mimeType"
      "resource" -> do
        resource <- o .: "resource"
        ResourcePart <$> resource .: "uri" <*> resource .:? "mimeType" <*> resource .:? "text"
      other      -> fail $ "Unknown content type: " <> show other

data CallToolResult = CallToolResult { ctrContent :: [ ContentPart ], ctrIsError :: Maybe Bool }
  deriving stock ( Eq, Show, Generic )

instance FromJSON CallToolResult where
  parseJSON
    = withObject "CallToolResult" $ \o -> CallToolResult <$> o .: "content" <*> o .:? "isError"

data ResourceInfo
  = ResourceInfo
  { riUri :: Text, riName :: Text, riDescription :: Maybe Text, riMimeType :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ResourceInfo where
  parseJSON = withObject "ResourceInfo" $ \o
    -> ResourceInfo <$> o .: "uri" <*> o .: "name" <*> o .:? "description" <*> o .:? "mimeType"

data ListResourcesResult
  = ListResourcesResult { lrrResources :: [ ResourceInfo ], lrrNextCursor :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ListResourcesResult where
  parseJSON = withObject "ListResourcesResult" $ \o
    -> ListResourcesResult <$> o .: "resources" <*> o .:? "nextCursor"

newtype ReadResourceParams = ReadResourceParams { rrpUri :: Text }
  deriving stock ( Eq, Show, Generic )

instance ToJSON ReadResourceParams where
  toJSON rrp = object [ "uri" .= rrpUri rrp ]

data ResourceContents
  = ResourceContents
  { resUri :: Text, resMimeType :: Maybe Text, resText :: Maybe Text, resBlob :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ResourceContents where
  parseJSON = withObject "ResourceContents" $ \o
    -> ResourceContents <$> o .: "uri" <*> o .:? "mimeType" <*> o .:? "text" <*> o .:? "blob"

newtype ReadResourceResult = ReadResourceResult { rrrContents :: [ ResourceContents ] }
  deriving stock ( Eq, Show, Generic )

instance FromJSON ReadResourceResult where
  parseJSON = withObject "ReadResourceResult" $ \o -> ReadResourceResult <$> o .: "contents"
