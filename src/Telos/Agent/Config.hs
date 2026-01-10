{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.Agent.Config ( AgentConfig(..), MCPServerConfig(..), defaultAgentConfig ) where

-- | Configuration for an MCP server connection
data MCPServerConfig
  = MCPServerConfig { mscName    :: Text            -- ^ Unique identifier for this server
                    , mscCommand :: FilePath        -- ^ Command to spawn the server
                    , mscArgs    :: [ String ]        -- ^ Command line arguments
                    , mscEnv     :: [ ( String, String ) ]  -- ^ Additional environment variables
                    }
  deriving stock ( Eq, Show, Generic )

-- | Configuration for the agent
data AgentConfig
  = AgentConfig
  { acMaxIterations :: Int                -- ^ Maximum tool call iterations (default 20)
  , acSystemPrompt :: Maybe Text         -- ^ Optional system prompt
  , acModel :: Text               -- ^ Model identifier (e.g. "gpt-4")
  , acMCPServers :: [ MCPServerConfig ]  -- ^ MCP servers to connect to
  , acStreamingEnabled :: Bool               -- ^ Enable streaming responses (default True)
  }
  deriving stock ( Eq, Show, Generic )

-- | Default agent configuration
defaultAgentConfig :: AgentConfig
defaultAgentConfig
  = AgentConfig { acMaxIterations = 20
                , acSystemPrompt = Nothing
                , acModel = "gpt-4"
                , acMCPServers = []
                , acStreamingEnabled = True
                }
