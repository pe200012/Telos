{-# LANGUAGE TemplateHaskell #-}

module Telos.MCP.Types
  ( ServerConfig
  , makeServerConfig
  , scName
  , scCommand
  , scArgs
  , scWorkDir
  , scEnv
  , ServerCapabilities
  , makeServerCapabilities
  , scapLogging
  , scapPrompts
  , scapResources
  , scapTools
  , ClientCapabilities
  , makeClientCapabilities
  , ccRoots
  , ccSampling
  , ClientInfo
  , makeClientInfo
  , ciName
  , ciVersion
  , ServerInfo
  , siName
  , siVersion
  , InitializeParams
  , makeInitializeParams
  , ipProtocolVersion
  , ipCapabilities
  , ipClientInfo
  , InitializeResult
  , irProtocolVersion
  , irCapabilities
  , irServerInfo
  , irInstructions
  , CallToolParams
  , makeCallToolParams
  , ctpName
  , ctpArguments
  , CallToolResult
  , ctrContent
  , ctrIsError
  , ListToolsResult
  , ltrTools
  , ltrNextCursor
  , ToolInfo
  , tiName
  , tiDescription
  , tiInputSchema
  , ListResourcesResult
  , lrrResources
  , lrrNextCursor
  , ResourceInfo
  , riUri
  , riName
  , riDescription
  , riMimeType
  , ReadResourceParams
  , makeReadResourceParams
  , rrpUri
  , ReadResourceResult
  , rrrContents
  , ResourceContents
  , resUri
  , resMimeType
  , resText
  , resBlob
  , ContentPart(..)
  , ipData
  , ipMimeType
  , rpUri
  , rpMimeType
  , rpText
  , ProtocolVersion
  , currentProtocolVersion
  , RootsCapability
  , makeRootsCapability
  , rcListChanged
  , SamplingCapability(..)
  , PromptsCapability
  , makePromptsCapability
  , pcListChanged
  , ResourcesCapability
  , makeResourcesCapability
  , rscSubscribe
  , rscListChanged
  , ToolsCapability
  , makeToolsCapability
  , toolsListChanged
  ) where

import           Data.Aeson
import           Data.Aeson.Types ( Parser )

import           Lens.Micro       ( (^.) )
import           Lens.Micro.TH    ( makeLenses )

type ProtocolVersion = Text

currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = "2024-11-05"

data ServerConfig = ServerConfig
  { _scName    :: Text
  , _scCommand :: FilePath
  , _scArgs    :: [ String ]
  , _scWorkDir :: Maybe FilePath
  , _scEnv     :: Maybe [ ( String, String ) ]
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ServerConfig

makeServerConfig :: Text -> FilePath -> [String] -> ServerConfig
makeServerConfig name cmd args = ServerConfig
  { _scName = name
  , _scCommand = cmd
  , _scArgs = args
  , _scWorkDir = Nothing
  , _scEnv = Nothing
  }

data ClientInfo = ClientInfo
  { _ciName    :: Text
  , _ciVersion :: Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ClientInfo

makeClientInfo :: Text -> Text -> ClientInfo
makeClientInfo name version = ClientInfo
  { _ciName = name
  , _ciVersion = version
  }

instance ToJSON ClientInfo where
  toJSON ci = object [ "name" .= (ci ^. ciName), "version" .= (ci ^. ciVersion) ]

data ServerInfo = ServerInfo
  { _siName    :: Text
  , _siVersion :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ServerInfo

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo <$> o .: "name" <*> o .:? "version"

-- Define capability types BEFORE ClientCapabilities which uses them
newtype RootsCapability = RootsCapability
  { _rcListChanged :: Maybe Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''RootsCapability

makeRootsCapability :: RootsCapability
makeRootsCapability = RootsCapability { _rcListChanged = Nothing }

data SamplingCapability = SamplingCapability
  deriving stock ( Eq, Show, Generic )

data ClientCapabilities = ClientCapabilities
  { _ccRoots    :: Maybe RootsCapability
  , _ccSampling :: Maybe SamplingCapability
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ClientCapabilities

makeClientCapabilities :: ClientCapabilities
makeClientCapabilities = ClientCapabilities
  { _ccRoots = Nothing
  , _ccSampling = Nothing
  }

instance ToJSON ClientCapabilities where
  toJSON cc = object
    $ maybe [] (\r -> [ "roots" .= r ]) (cc ^. ccRoots)
    ++ maybe [] (\s -> [ "sampling" .= s ]) (cc ^. ccSampling)

instance ToJSON RootsCapability where
  toJSON rc = object $ maybe [] (\b -> [ "listChanged" .= b ]) (rc ^. rcListChanged)

instance ToJSON SamplingCapability where
  toJSON _ = object []

newtype PromptsCapability = PromptsCapability
  { _pcListChanged :: Maybe Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''PromptsCapability

makePromptsCapability :: PromptsCapability
makePromptsCapability = PromptsCapability { _pcListChanged = Nothing }

data ResourcesCapability = ResourcesCapability
  { _rscSubscribe   :: Maybe Bool
  , _rscListChanged :: Maybe Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ResourcesCapability

makeResourcesCapability :: ResourcesCapability
makeResourcesCapability = ResourcesCapability
  { _rscSubscribe = Nothing
  , _rscListChanged = Nothing
  }

newtype ToolsCapability = ToolsCapability
  { _toolsListChanged :: Maybe Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ToolsCapability

makeToolsCapability :: ToolsCapability
makeToolsCapability = ToolsCapability { _toolsListChanged = Nothing }

data ServerCapabilities = ServerCapabilities
  { _scapLogging   :: Maybe Bool
  , _scapPrompts   :: Maybe PromptsCapability
  , _scapResources :: Maybe ResourcesCapability
  , _scapTools     :: Maybe ToolsCapability
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ServerCapabilities

makeServerCapabilities :: ServerCapabilities
makeServerCapabilities = ServerCapabilities
  { _scapLogging = Nothing
  , _scapPrompts = Nothing
  , _scapResources = Nothing
  , _scapTools = Nothing
  }

instance FromJSON ServerCapabilities where
  parseJSON = withObject "ServerCapabilities" $ \o -> ServerCapabilities
    <$> o .:? "logging"
    <*> o .:? "prompts"
    <*> o .:? "resources"
    <*> o .:? "tools"

instance FromJSON PromptsCapability where
  parseJSON = withObject "PromptsCapability" $ \o -> PromptsCapability <$> o .:? "listChanged"

instance FromJSON ResourcesCapability where
  parseJSON = withObject "ResourcesCapability" $ \o ->
    ResourcesCapability <$> o .:? "subscribe" <*> o .:? "listChanged"

instance FromJSON ToolsCapability where
  parseJSON = withObject "ToolsCapability" $ \o -> ToolsCapability <$> o .:? "listChanged"

data InitializeParams = InitializeParams
  { _ipProtocolVersion :: ProtocolVersion
  , _ipCapabilities    :: ClientCapabilities
  , _ipClientInfo      :: ClientInfo
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''InitializeParams

makeInitializeParams :: ClientInfo -> InitializeParams
makeInitializeParams info = InitializeParams
  { _ipProtocolVersion = currentProtocolVersion
  , _ipCapabilities = makeClientCapabilities
  , _ipClientInfo = info
  }

instance ToJSON InitializeParams where
  toJSON ip = object
    [ "protocolVersion" .= (ip ^. ipProtocolVersion)
    , "capabilities" .= (ip ^. ipCapabilities)
    , "clientInfo" .= (ip ^. ipClientInfo)
    ]

data InitializeResult = InitializeResult
  { _irProtocolVersion :: ProtocolVersion
  , _irCapabilities    :: ServerCapabilities
  , _irServerInfo      :: Maybe ServerInfo
  , _irInstructions    :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''InitializeResult

instance FromJSON InitializeResult where
  parseJSON = withObject "InitializeResult" $ \o -> InitializeResult
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .:? "serverInfo"
    <*> o .:? "instructions"

data ToolInfo = ToolInfo
  { _tiName        :: Text
  , _tiDescription :: Maybe Text
  , _tiInputSchema :: Value
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ToolInfo

instance FromJSON ToolInfo where
  parseJSON = withObject "ToolInfo" $ \o ->
    ToolInfo <$> o .: "name" <*> o .:? "description" <*> o .: "inputSchema"

data ListToolsResult = ListToolsResult
  { _ltrTools      :: [ ToolInfo ]
  , _ltrNextCursor :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ListToolsResult

instance FromJSON ListToolsResult where
  parseJSON = withObject "ListToolsResult" $ \o ->
    ListToolsResult <$> o .: "tools" <*> o .:? "nextCursor"

data CallToolParams = CallToolParams
  { _ctpName      :: Text
  , _ctpArguments :: Maybe Value
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''CallToolParams

makeCallToolParams :: Text -> Maybe Value -> CallToolParams
makeCallToolParams name args = CallToolParams
  { _ctpName = name
  , _ctpArguments = args
  }

instance ToJSON CallToolParams where
  toJSON ctp = object
    $ ("name" .= (ctp ^. ctpName)) : maybe [] (\a -> [ "arguments" .= a ]) (ctp ^. ctpArguments)

data ContentPart
  = TextPart Text
  | ImagePart { _ipData :: Text, _ipMimeType :: Text }
  | ResourcePart { _rpUri :: Text, _rpMimeType :: Maybe Text, _rpText :: Maybe Text }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ContentPart

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

data CallToolResult = CallToolResult
  { _ctrContent :: [ ContentPart ]
  , _ctrIsError :: Maybe Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''CallToolResult

instance FromJSON CallToolResult where
  parseJSON = withObject "CallToolResult" $ \o ->
    CallToolResult <$> o .: "content" <*> o .:? "isError"

data ResourceInfo = ResourceInfo
  { _riUri         :: Text
  , _riName        :: Text
  , _riDescription :: Maybe Text
  , _riMimeType    :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ResourceInfo

instance FromJSON ResourceInfo where
  parseJSON = withObject "ResourceInfo" $ \o ->
    ResourceInfo <$> o .: "uri" <*> o .: "name" <*> o .:? "description" <*> o .:? "mimeType"

data ListResourcesResult = ListResourcesResult
  { _lrrResources  :: [ ResourceInfo ]
  , _lrrNextCursor :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ListResourcesResult

instance FromJSON ListResourcesResult where
  parseJSON = withObject "ListResourcesResult" $ \o ->
    ListResourcesResult <$> o .: "resources" <*> o .:? "nextCursor"

newtype ReadResourceParams = ReadResourceParams
  { _rrpUri :: Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ReadResourceParams

makeReadResourceParams :: Text -> ReadResourceParams
makeReadResourceParams uri = ReadResourceParams { _rrpUri = uri }

instance ToJSON ReadResourceParams where
  toJSON rrp = object [ "uri" .= (rrp ^. rrpUri) ]

data ResourceContents = ResourceContents
  { _resUri      :: Text
  , _resMimeType :: Maybe Text
  , _resText     :: Maybe Text
  , _resBlob     :: Maybe Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ResourceContents

instance FromJSON ResourceContents where
  parseJSON = withObject "ResourceContents" $ \o ->
    ResourceContents <$> o .: "uri" <*> o .:? "mimeType" <*> o .:? "text" <*> o .:? "blob"

newtype ReadResourceResult = ReadResourceResult
  { _rrrContents :: [ ResourceContents ]
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ReadResourceResult

instance FromJSON ReadResourceResult where
  parseJSON = withObject "ReadResourceResult" $ \o -> ReadResourceResult <$> o .: "contents"
