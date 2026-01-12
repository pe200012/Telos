{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Config
  ( AgentConfig
  , makeAgentConfig
  , acMaxIterations
  , acPromptConfig
  , acModel
  , acMCPServers
  , acStreamingEnabled
  , acPruneConfig
  , MCPServerConfig
  , makeMCPServerConfig
  , mscName
  , mscCommand
  , mscArgs
  , mscEnv
  , defaultAgentConfig
  ) where

import           Lens.Micro.TH        ( makeLenses )

import           Relude

import           Telos.Context.Types  ( PruneConfig, defaultPruneConfig )
import           Telos.Prompt.Types   ( SystemPromptConfig )

data MCPServerConfig
  = MCPServerConfig { _mscName    :: Text
                    , _mscCommand :: FilePath
                    , _mscArgs    :: [ String ]
                    , _mscEnv     :: [ ( String, String ) ]
                    }
  deriving stock ( Eq, Show, Generic )

makeLenses ''MCPServerConfig

makeMCPServerConfig :: Text -> FilePath -> [ String ] -> MCPServerConfig
makeMCPServerConfig name cmd args
  = MCPServerConfig { _mscName = name, _mscCommand = cmd, _mscArgs = args, _mscEnv = [] }

data AgentConfig
  = AgentConfig { _acMaxIterations    :: Int
                , _acPromptConfig     :: Maybe SystemPromptConfig
                , _acModel            :: Text
                , _acMCPServers       :: [ MCPServerConfig ]
                , _acStreamingEnabled :: Bool
                , _acPruneConfig      :: PruneConfig
                }
  deriving stock ( Eq, Show, Generic )

makeLenses ''AgentConfig

makeAgentConfig :: Text -> AgentConfig
makeAgentConfig model
  = AgentConfig { _acMaxIterations    = 20
                , _acPromptConfig     = Nothing
                , _acModel            = model
                , _acMCPServers       = []
                , _acStreamingEnabled = True
                , _acPruneConfig      = defaultPruneConfig
                }

defaultAgentConfig :: AgentConfig
defaultAgentConfig
  = AgentConfig { _acMaxIterations    = 20
                , _acPromptConfig     = Nothing
                , _acModel            = "gpt-4"
                , _acMCPServers       = []
                , _acStreamingEnabled = True
                , _acPruneConfig      = defaultPruneConfig
                }
