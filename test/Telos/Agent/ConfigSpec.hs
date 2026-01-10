module Telos.Agent.ConfigSpec ( spec ) where

import Test.Hspec

import Telos.Agent.Config

spec :: Spec
spec = do
  describe "AgentConfig" $ do
    describe "defaultAgentConfig" $ do
      it "has maxIterations of 20" $ do
        acMaxIterations defaultAgentConfig `shouldBe` 20

      it "has no system prompt by default" $ do
        acSystemPrompt defaultAgentConfig `shouldBe` Nothing

      it "has default model gpt-4" $ do
        acModel defaultAgentConfig `shouldBe` "gpt-4"

      it "has no MCP servers by default" $ do
        acMCPServers defaultAgentConfig `shouldBe` []

  describe "MCPServerConfig" $ do
    it "can be constructed with all fields" $ do
      let cfg = MCPServerConfig
            { mscName = "test-server"
            , mscCommand = "/usr/bin/test"
            , mscArgs = ["--arg1", "--arg2"]
            , mscEnv = [("KEY", "VALUE")]
            }
      mscName cfg `shouldBe` "test-server"
      mscCommand cfg `shouldBe` "/usr/bin/test"
      mscArgs cfg `shouldBe` ["--arg1", "--arg2"]
      mscEnv cfg `shouldBe` [("KEY", "VALUE")]
