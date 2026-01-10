{-# LANGUAGE TemplateHaskell #-}

module Telos.CLI.Config
  ( CliConfig
  , makeCliConfig
  , ccModel
  , ccMaxIterations
  , ccSystemPrompt
  , ccMcpServers
  , ccStreamingEnabled
  , McpServerEntry
  , makeMcpServerEntry
  , mseName
  , mseCommand
  , mseArgs
  , mseWorkDir
  , mseEnv
  , defaultCliConfig
  , loadConfig
  , configFilePath
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Lens.Micro           ( (^.) )
import           Lens.Micro.TH        ( makeLenses )
import           System.Directory     ( doesFileExist, getXdgDirectory
                                      , XdgDirectory(XdgConfig) )
import           System.FilePath      ( (</>) )

-- | MCP Server entry in config file
data McpServerEntry = McpServerEntry
  { _mseName    :: Text
  , _mseCommand :: FilePath
  , _mseArgs    :: [String]
  , _mseWorkDir :: Maybe FilePath
  , _mseEnv     :: Maybe [(String, String)]
  }
  deriving stock (Eq, Show, Generic)

makeLenses ''McpServerEntry

makeMcpServerEntry :: Text -> FilePath -> [String] -> McpServerEntry
makeMcpServerEntry name cmd args = McpServerEntry
  { _mseName = name
  , _mseCommand = cmd
  , _mseArgs = args
  , _mseWorkDir = Nothing
  , _mseEnv = Nothing
  }

instance FromJSON McpServerEntry where
  parseJSON = withObject "McpServerEntry" $ \o -> McpServerEntry
    <$> o .: "name"
    <*> o .: "command"
    <*> o .:? "args" .!= []
    <*> o .:? "workDir"
    <*> o .:? "env"

instance ToJSON McpServerEntry where
  toJSON mse = object
    [ "name" .= (mse ^. mseName)
    , "command" .= (mse ^. mseCommand)
    , "args" .= (mse ^. mseArgs)
    , "workDir" .= (mse ^. mseWorkDir)
    , "env" .= (mse ^. mseEnv)
    ]

-- | CLI configuration
data CliConfig = CliConfig
  { _ccModel            :: Text
  , _ccMaxIterations    :: Int
  , _ccSystemPrompt     :: Maybe Text
  , _ccMcpServers       :: [McpServerEntry]
  , _ccStreamingEnabled :: Bool
  }
  deriving stock (Eq, Show, Generic)

makeLenses ''CliConfig

makeCliConfig :: Text -> CliConfig
makeCliConfig model = CliConfig
  { _ccModel = model
  , _ccMaxIterations = 20
  , _ccSystemPrompt = Nothing
  , _ccMcpServers = []
  , _ccStreamingEnabled = True
  }

defaultCliConfig :: CliConfig
defaultCliConfig = CliConfig
  { _ccModel = "gpt-4o"
  , _ccMaxIterations = 20
  , _ccSystemPrompt = Just "You are a helpful assistant with access to tools."
  , _ccMcpServers = []
  , _ccStreamingEnabled = True
  }

instance FromJSON CliConfig where
  parseJSON = withObject "CliConfig" $ \o -> CliConfig
    <$> o .:? "model" .!= "gpt-4o"
    <*> o .:? "maxIterations" .!= 20
    <*> o .:? "systemPrompt"
    <*> o .:? "mcpServers" .!= []
    <*> o .:? "streamingEnabled" .!= True

instance ToJSON CliConfig where
  toJSON cfg = object
    [ "model" .= (cfg ^. ccModel)
    , "maxIterations" .= (cfg ^. ccMaxIterations)
    , "systemPrompt" .= (cfg ^. ccSystemPrompt)
    , "mcpServers" .= (cfg ^. ccMcpServers)
    , "streamingEnabled" .= (cfg ^. ccStreamingEnabled)
    ]

-- | Get the config file path (~/.config/telos/config.json)
configFilePath :: IO FilePath
configFilePath = do
  configDir <- getXdgDirectory XdgConfig "telos"
  pure $ configDir </> "config.json"

-- | Load config from file, or return default if not found
loadConfig :: IO CliConfig
loadConfig = do
  path <- configFilePath
  exists <- doesFileExist path
  if exists
    then do
      content <- LBS.readFile path
      case eitherDecode content of
        Left err  -> do
          putStrLn $ "Warning: Failed to parse config file: " <> err
          putStrLn "Using default configuration."
          pure defaultCliConfig
        Right cfg -> pure cfg
    else pure defaultCliConfig
