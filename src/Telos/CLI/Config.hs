{-# LANGUAGE TemplateHaskell #-}

module Telos.CLI.Config
  ( CliConfig
  , makeCliConfig
  , ccModel
  , ccMaxIterations
  , ccSystemPrompt
  , ccMcpServers
  , ccStreamingEnabled
  , ccSnapshotEnabled
  , ccPruneConfig
  , defaultPruneConfig
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
  , fromTelosConfig
  , TelosConfig
  , loadTelosConfig
  ) where

import           Data.Aeson
import qualified Data.Map.Strict      as Map

import           Control.Lens           ( (^.) )
import           Control.Lens        ( makeLenses )

import           Relude

import           System.Directory     ( XdgDirectory(XdgConfig), getXdgDirectory )
import           System.FilePath      ( (</>) )

import           Telos.Context.Types  ( PruneConfig(..), defaultPruneConfig )
import qualified Telos.Config.Load    as Config
import           Telos.Config.Types   ( TelosConfig, McpConfig(..), tcModel, tcMaxIterations
                                      , tcMcp, tcStreamingEnabled, tcSnapshotEnabled
                                      , tcCompaction, ccPrune
                                      , mcCommand, mcArgs, mcEnv, mcWorkDir )

data McpServerEntry
  = McpServerEntry { _mseName    :: Text
                   , _mseCommand :: FilePath
                   , _mseArgs    :: [ String ]
                   , _mseWorkDir :: Maybe FilePath
                   , _mseEnv     :: Maybe [ ( String, String ) ]
                   }
  deriving stock ( Eq, Show, Generic )

makeLenses ''McpServerEntry

makeMcpServerEntry :: Text -> FilePath -> [ String ] -> McpServerEntry
makeMcpServerEntry name cmd args
  = McpServerEntry
  { _mseName = name, _mseCommand = cmd, _mseArgs = args, _mseWorkDir = Nothing, _mseEnv = Nothing }

instance FromJSON McpServerEntry where
  parseJSON = withObject "McpServerEntry" $ \o -> McpServerEntry <$> o .: "name"
    <*> o .: "command"
    <*> o .:? "args" .!= []
    <*> o .:? "workDir"
    <*> o .:? "env"

instance ToJSON McpServerEntry where
  toJSON mse
    = object
      [ "name" .= (mse ^. mseName)
      , "command" .= (mse ^. mseCommand)
      , "args" .= (mse ^. mseArgs)
      , "workDir" .= (mse ^. mseWorkDir)
      , "env" .= (mse ^. mseEnv)
      ]

data CliConfig
  = CliConfig { _ccModel :: Text
              , _ccMaxIterations :: Int
              , _ccSystemPrompt :: Maybe Text
              , _ccMcpServers :: [ McpServerEntry ]
              , _ccStreamingEnabled :: Bool
              , _ccSnapshotEnabled :: Bool
              , _ccPruneConfig :: PruneConfig
              }
  deriving stock ( Eq, Show, Generic )

makeLenses ''CliConfig

makeCliConfig :: Text -> CliConfig
makeCliConfig model
  = CliConfig { _ccModel = model
              , _ccMaxIterations = 20
              , _ccSystemPrompt = Nothing
              , _ccMcpServers = []
              , _ccStreamingEnabled = True
              , _ccSnapshotEnabled = True
              , _ccPruneConfig = defaultPruneConfig
              }

defaultCliConfig :: CliConfig
defaultCliConfig
  = CliConfig { _ccModel = "gpt-4o"
              , _ccMaxIterations = 20
              , _ccSystemPrompt = Just "You are a helpful assistant with access to tools."
              , _ccMcpServers = []
              , _ccStreamingEnabled = True
              , _ccSnapshotEnabled = True
              , _ccPruneConfig = defaultPruneConfig
              }

instance FromJSON CliConfig where
  parseJSON = withObject "CliConfig" $ \o -> CliConfig <$> o .:? "model" .!= "gpt-4o"
    <*> o .:? "maxIterations" .!= 20
    <*> o .:? "systemPrompt"
    <*> o .:? "mcpServers" .!= []
    <*> o .:? "streamingEnabled" .!= True
    <*> o .:? "snapshotEnabled" .!= True
    <*> o .:? "pruneConfig" .!= defaultPruneConfig

instance ToJSON CliConfig where
  toJSON cfg
    = object
      [ "model" .= (cfg ^. ccModel)
      , "maxIterations" .= (cfg ^. ccMaxIterations)
      , "systemPrompt" .= (cfg ^. ccSystemPrompt)
      , "mcpServers" .= (cfg ^. ccMcpServers)
      , "streamingEnabled" .= (cfg ^. ccStreamingEnabled)
      , "snapshotEnabled" .= (cfg ^. ccSnapshotEnabled)
      , "pruneConfig" .= (cfg ^. ccPruneConfig)
      ]

configFilePath :: IO FilePath
configFilePath = do
  configDir <- getXdgDirectory XdgConfig "telos"
  pure $ configDir </> "config.json"

loadTelosConfig :: IO TelosConfig
loadTelosConfig = Config.loadConfig

fromTelosConfig :: TelosConfig -> CliConfig
fromTelosConfig tc = CliConfig
  { _ccModel = tc ^. tcModel
  , _ccMaxIterations = tc ^. tcMaxIterations
  , _ccSystemPrompt = Just "You are a helpful assistant with access to tools."
  , _ccMcpServers = mcpToEntries (tc ^. tcMcp)
  , _ccStreamingEnabled = tc ^. tcStreamingEnabled
  , _ccSnapshotEnabled = tc ^. tcSnapshotEnabled
  , _ccPruneConfig = compactionToPrune (tc ^. tcCompaction)
  }
  where
    mcpToEntries :: Map Text McpConfig -> [McpServerEntry]
    mcpToEntries = Map.foldrWithKey toEntry []
    
    toEntry :: Text -> McpConfig -> [McpServerEntry] -> [McpServerEntry]
    toEntry name cfg acc = case cfg of
      McpLocal{} -> McpServerEntry
        { _mseName = name
        , _mseCommand = toString (cfg ^. mcCommand)
        , _mseArgs = map toString (cfg ^. mcArgs)
        , _mseWorkDir = toString <$> (cfg ^. mcWorkDir)
        , _mseEnv = Just $ envToList (cfg ^. mcEnv)
        } : acc
      McpRemote{} -> acc
    
    envToList :: Map Text Text -> [(String, String)]
    envToList = Map.foldrWithKey (\k v acc -> (toString k, toString v) : acc) []
    
    compactionToPrune tc' = defaultPruneConfig
      { _pcEnabled = tc' ^. ccPrune
      }

loadConfig :: IO CliConfig
loadConfig = fromTelosConfig <$> loadTelosConfig
