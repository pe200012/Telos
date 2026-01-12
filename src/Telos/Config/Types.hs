{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.Config.Types
    ( -- * Core Config
      TelosConfig(..)
    , tcModel
    , tcSmallModel
    , tcProviders
    , tcMcp
    , tcPermissions
    , tcInstructions
    , tcCompaction
    , tcMaxIterations
    , tcStreamingEnabled
    , tcSnapshotEnabled
    , tcLogLevel
      -- * Provider Config
    , ProviderConfig(..)
    , pcApiKey
    , pcBaseURL
    , pcTimeout
    , defaultProviderConfig
      -- * MCP Config
    , McpConfig(..)
    , mcName
    , mcEnabled
    , mcTimeout
    , mcCommand
    , mcArgs
    , mcEnv
    , mcWorkDir
    , mcUrl
    , mcHeaders
      -- * Permission
    , Permission(..)
    , permissionFromText
    , permissionToText
      -- * Compaction Config
    , CompactionConfig(..)
    , ccAuto
    , ccPrune
    , ccThreshold
    , defaultCompactionConfig
      -- * Log Level
    , LogLevel(..)
    , logLevelFromText
    , logLevelToText
      -- * Defaults
    , defaultTelosConfig
    ) where

import Relude

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Control.Lens (makeLenses)


-- | Permission level for tool execution
data Permission
    = Ask   -- ^ Ask user for confirmation
    | Allow -- ^ Always allow
    | Deny  -- ^ Always deny
    deriving stock (Show, Eq, Generic)

permissionFromText :: Text -> Maybe Permission
permissionFromText = \case
    "ask"   -> Just Ask
    "allow" -> Just Allow
    "deny"  -> Just Deny
    _       -> Nothing

permissionToText :: Permission -> Text
permissionToText = \case
    Ask   -> "ask"
    Allow -> "allow"
    Deny  -> "deny"

instance FromJSON Permission where
    parseJSON = withText "Permission" $ \t ->
        case permissionFromText t of
            Just p  -> pure p
            Nothing -> fail $ "Invalid permission: " <> toString t <> ". Expected: ask, allow, deny"

instance ToJSON Permission where
    toJSON = String . permissionToText


-- | Log level
data LogLevel
    = LogDebug
    | LogInfo
    | LogWarn
    | LogError
    deriving stock (Show, Eq, Ord, Generic)

logLevelFromText :: Text -> Maybe LogLevel
logLevelFromText = \case
    "debug" -> Just LogDebug
    "info"  -> Just LogInfo
    "warn"  -> Just LogWarn
    "error" -> Just LogError
    _       -> Nothing

logLevelToText :: LogLevel -> Text
logLevelToText = \case
    LogDebug -> "debug"
    LogInfo  -> "info"
    LogWarn  -> "warn"
    LogError -> "error"

instance FromJSON LogLevel where
    parseJSON = withText "LogLevel" $ \t ->
        case logLevelFromText t of
            Just l  -> pure l
            Nothing -> fail $ "Invalid log level: " <> toString t <> ". Expected: debug, info, warn, error"

instance ToJSON LogLevel where
    toJSON = String . logLevelToText


-- | Provider configuration (OpenAI, Anthropic, etc.)
data ProviderConfig = ProviderConfig
    { _pcApiKey  :: Maybe Text  -- ^ API key (can use ${ENV_VAR} syntax)
    , _pcBaseURL :: Maybe Text  -- ^ Base URL for API
    , _pcTimeout :: Maybe Int   -- ^ Request timeout in seconds
    }
    deriving stock (Show, Eq, Generic)

makeLenses ''ProviderConfig

defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
    { _pcApiKey  = Nothing
    , _pcBaseURL = Nothing
    , _pcTimeout = Nothing
    }

instance FromJSON ProviderConfig where
    parseJSON = withObject "ProviderConfig" $ \o -> ProviderConfig
        <$> o .:? "api_key"
        <*> o .:? "base_url"
        <*> o .:? "timeout"

instance ToJSON ProviderConfig where
    toJSON ProviderConfig{..} = object $ catMaybes
        [ ("api_key" .=)  <$> _pcApiKey
        , ("base_url" .=) <$> _pcBaseURL
        , ("timeout" .=)  <$> _pcTimeout
        ]


-- | MCP server configuration
data McpConfig
    = McpLocal
        { _mcName    :: Text
        , _mcCommand :: Text
        , _mcArgs    :: [Text]
        , _mcEnv     :: Map Text Text
        , _mcWorkDir :: Maybe FilePath
        , _mcEnabled :: Bool
        , _mcTimeout :: Maybe Int
        }
    | McpRemote
        { _mcName    :: Text
        , _mcUrl     :: Text
        , _mcHeaders :: Map Text Text
        , _mcEnabled :: Bool
        , _mcTimeout :: Maybe Int
        }
    deriving stock (Show, Eq, Generic)

makeLenses ''McpConfig

instance FromJSON McpConfig where
    parseJSON = withObject "McpConfig" $ \o -> do
        name <- o .: "name"
        mcType <- o .:? "type" .!= ("local" :: Text)
        enabled <- o .:? "enabled" .!= True
        timeout <- o .:? "timeout"
        case mcType of
            "local" -> McpLocal name
                <$> o .: "command"
                <*> o .:? "args" .!= []
                <*> o .:? "env" .!= Map.empty
                <*> o .:? "work_dir"
                <*> pure enabled
                <*> pure timeout
            "remote" -> McpRemote name
                <$> o .: "url"
                <*> o .:? "headers" .!= Map.empty
                <*> pure enabled
                <*> pure timeout
            _ -> fail $ "Invalid MCP type: " <> toString mcType <> ". Expected: local, remote"

instance ToJSON McpConfig where
    toJSON (McpLocal name cmd args env workDir enabled timeout) = object $ catMaybes
        [ Just $ "name" .= name
        , Just $ "type" .= ("local" :: Text)
        , Just $ "command" .= cmd
        , if null args then Nothing else Just $ "args" .= args
        , if Map.null env then Nothing else Just $ "env" .= env
        , ("work_dir" .=) <$> workDir
        , if enabled then Nothing else Just $ "enabled" .= enabled
        , ("timeout" .=) <$> timeout
        ]
    toJSON (McpRemote name url headers enabled timeout) = object $ catMaybes
        [ Just $ "name" .= name
        , Just $ "type" .= ("remote" :: Text)
        , Just $ "url" .= url
        , if Map.null headers then Nothing else Just $ "headers" .= headers
        , if enabled then Nothing else Just $ "enabled" .= enabled
        , ("timeout" .=) <$> timeout
        ]


-- | Compaction/DCP configuration
data CompactionConfig = CompactionConfig
    { _ccAuto      :: Bool    -- ^ Automatically compact when threshold reached
    , _ccPrune     :: Bool    -- ^ Enable DCP pruning
    , _ccThreshold :: Double  -- ^ Context usage threshold to trigger (0.0-1.0)
    }
    deriving stock (Show, Eq, Generic)

makeLenses ''CompactionConfig

defaultCompactionConfig :: CompactionConfig
defaultCompactionConfig = CompactionConfig
    { _ccAuto      = True
    , _ccPrune     = True
    , _ccThreshold = 0.8
    }

instance FromJSON CompactionConfig where
    parseJSON = withObject "CompactionConfig" $ \o -> CompactionConfig
        <$> o .:? "auto" .!= True
        <*> o .:? "prune" .!= True
        <*> o .:? "threshold" .!= 0.8

instance ToJSON CompactionConfig where
    toJSON CompactionConfig{..} = object
        [ "auto" .= _ccAuto
        , "prune" .= _ccPrune
        , "threshold" .= _ccThreshold
        ]


-- | Root configuration type
data TelosConfig = TelosConfig
    { _tcModel            :: Text                   -- ^ Model in "provider/model" format
    , _tcSmallModel       :: Maybe Text             -- ^ Small model for summaries
    , _tcProviders        :: Map Text ProviderConfig -- ^ Provider configurations
    , _tcMcp              :: Map Text McpConfig     -- ^ MCP server configurations
    , _tcPermissions      :: Map Text Permission    -- ^ Tool permissions
    , _tcInstructions     :: [FilePath]             -- ^ Additional instruction files
    , _tcCompaction       :: CompactionConfig       -- ^ DCP/compaction settings
    , _tcMaxIterations    :: Int                    -- ^ Max agent iterations
    , _tcStreamingEnabled :: Bool                   -- ^ Enable streaming output
    , _tcSnapshotEnabled  :: Bool                   -- ^ Enable session snapshots
    , _tcLogLevel         :: LogLevel               -- ^ Log level
    }
    deriving stock (Show, Eq, Generic)

makeLenses ''TelosConfig

defaultTelosConfig :: TelosConfig
defaultTelosConfig = TelosConfig
    { _tcModel            = "copilot/gpt-4o"
    , _tcSmallModel       = Nothing
    , _tcProviders        = Map.empty
    , _tcMcp              = Map.empty
    , _tcPermissions      = defaultPermissions
    , _tcInstructions     = []
    , _tcCompaction       = defaultCompactionConfig
    , _tcMaxIterations    = 50
    , _tcStreamingEnabled = True
    , _tcSnapshotEnabled  = True
    , _tcLogLevel         = LogInfo
    }
  where
    defaultPermissions = Map.fromList
        [ ("read", Allow)
        , ("glob", Allow)
        , ("grep", Allow)
        , ("edit", Ask)
        , ("write", Ask)
        , ("bash", Ask)
        , ("webfetch", Ask)
        ]

instance FromJSON TelosConfig where
    parseJSON = withObject "TelosConfig" $ \o -> do
        model <- o .:? "model" .!= _tcModel defaultTelosConfig
        smallModel <- o .:? "small_model"
        providers <- o .:? "providers" .!= Map.empty
        
        -- MCP: parse as map with name injection
        mcpObj <- o .:? "mcp" .!= KM.empty
        mcp <- parseMcpMap mcpObj
        
        permissions <- o .:? "permissions" .!= _tcPermissions defaultTelosConfig
        instructions <- o .:? "instructions" .!= []
        compaction <- o .:? "compaction" .!= defaultCompactionConfig
        maxIter <- o .:? "max_iterations" .!= _tcMaxIterations defaultTelosConfig
        streaming <- o .:? "streaming_enabled" .!= _tcStreamingEnabled defaultTelosConfig
        snapshot <- o .:? "snapshot_enabled" .!= _tcSnapshotEnabled defaultTelosConfig
        logLevel <- o .:? "log_level" .!= _tcLogLevel defaultTelosConfig
        
        pure TelosConfig
            { _tcModel            = model
            , _tcSmallModel       = smallModel
            , _tcProviders        = providers
            , _tcMcp              = mcp
            , _tcPermissions      = permissions
            , _tcInstructions     = instructions
            , _tcCompaction       = compaction
            , _tcMaxIterations    = maxIter
            , _tcStreamingEnabled = streaming
            , _tcSnapshotEnabled  = snapshot
            , _tcLogLevel         = logLevel
            }
      where
        parseMcpMap :: Object -> Parser (Map Text McpConfig)
        parseMcpMap obj = do
            let kvPairs = KM.toList obj
            Map.fromList <$> traverse parseMcpPair kvPairs
        
        parseMcpPair (k, v) = do
            let name = Key.toText k
            cfg <- withObject "McpConfig" (parseMcpWithName name) v
            pure (name, cfg)
        
        parseMcpWithName :: Text -> Object -> Parser McpConfig
        parseMcpWithName name o = do
            mcType <- o .:? "type" .!= ("local" :: Text)
            enabled <- o .:? "enabled" .!= True
            timeout <- o .:? "timeout"
            case mcType of
                "local" -> McpLocal name
                    <$> o .: "command"
                    <*> o .:? "args" .!= []
                    <*> o .:? "env" .!= Map.empty
                    <*> o .:? "work_dir"
                    <*> pure enabled
                    <*> pure timeout
                "remote" -> McpRemote name
                    <$> o .: "url"
                    <*> o .:? "headers" .!= Map.empty
                    <*> pure enabled
                    <*> pure timeout
                _ -> fail $ "Invalid MCP type: " <> toString mcType

instance ToJSON TelosConfig where
    toJSON TelosConfig{..} = object $ catMaybes
        [ Just $ "model" .= _tcModel
        , ("small_model" .=) <$> _tcSmallModel
        , if Map.null _tcProviders then Nothing else Just $ "providers" .= _tcProviders
        , if Map.null _tcMcp then Nothing else Just $ "mcp" .= mcpToObject _tcMcp
        , if Map.null _tcPermissions 
            then Nothing 
            else Just $ "permissions" .= _tcPermissions
        , if null _tcInstructions then Nothing else Just $ "instructions" .= _tcInstructions
        , if _tcCompaction == defaultCompactionConfig 
            then Nothing 
            else Just $ "compaction" .= _tcCompaction
        , if _tcMaxIterations == 50 
            then Nothing 
            else Just $ "max_iterations" .= _tcMaxIterations
        , if _tcStreamingEnabled 
            then Nothing 
            else Just $ "streaming_enabled" .= _tcStreamingEnabled
        , if _tcSnapshotEnabled 
            then Nothing 
            else Just $ "snapshot_enabled" .= _tcSnapshotEnabled
        , if _tcLogLevel == LogInfo 
            then Nothing 
            else Just $ "log_level" .= _tcLogLevel
        ]
      where
        mcpToObject :: Map Text McpConfig -> Value
        mcpToObject m = Object $ KM.fromList 
            [ (Key.fromText k, mcpToValue v) | (k, v) <- Map.toList m ]
        
        mcpToValue :: McpConfig -> Value
        mcpToValue (McpLocal _ cmd args env workDir enabled timeout) = object $ catMaybes
            [ Just $ "type" .= ("local" :: Text)
            , Just $ "command" .= cmd
            , if null args then Nothing else Just $ "args" .= args
            , if Map.null env then Nothing else Just $ "env" .= env
            , ("work_dir" .=) <$> workDir
            , if enabled then Nothing else Just $ "enabled" .= enabled
            , ("timeout" .=) <$> timeout
            ]
        mcpToValue (McpRemote _ url headers enabled timeout) = object $ catMaybes
            [ Just $ "type" .= ("remote" :: Text)
            , Just $ "url" .= url
            , if Map.null headers then Nothing else Just $ "headers" .= headers
            , if enabled then Nothing else Just $ "enabled" .= enabled
            , ("timeout" .=) <$> timeout
            ]
