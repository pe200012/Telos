{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Config
  ( AgentConfig
  , makeAgentConfig
  , acMaxIterations
  , acSystemPrompt
  , acModel
  , acMCPServers
  , acStreamingEnabled
  , MCPServerConfig
  , makeMCPServerConfig
  , mscName
  , mscCommand
  , mscArgs
  , mscEnv
  , defaultAgentConfig
  ) where

import           Lens.Micro.TH ( makeLenses )

data MCPServerConfig = MCPServerConfig
  { _mscName    :: Text
  , _mscCommand :: FilePath
  , _mscArgs    :: [ String ]
  , _mscEnv     :: [ ( String, String ) ]
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''MCPServerConfig

makeMCPServerConfig :: Text -> FilePath -> [ String ] -> MCPServerConfig
makeMCPServerConfig name cmd args = MCPServerConfig
  { _mscName = name
  , _mscCommand = cmd
  , _mscArgs = args
  , _mscEnv = []
  }

data AgentConfig = AgentConfig
  { _acMaxIterations    :: Int
  , _acSystemPrompt     :: Maybe Text
  , _acModel            :: Text
  , _acMCPServers       :: [ MCPServerConfig ]
  , _acStreamingEnabled :: Bool
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''AgentConfig

makeAgentConfig :: Text -> AgentConfig
makeAgentConfig model = AgentConfig
  { _acMaxIterations = 20
  , _acSystemPrompt = Nothing
  , _acModel = model
  , _acMCPServers = []
  , _acStreamingEnabled = True
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig
  { _acMaxIterations    = 20
  , _acSystemPrompt     = Nothing
  , _acModel            = "gpt-4"
  , _acMCPServers       = []
  , _acStreamingEnabled = True
  }
