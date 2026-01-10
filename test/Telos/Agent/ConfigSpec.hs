module Telos.Agent.ConfigSpec ( spec ) where

import           Lens.Micro         ( (.~), (^.) )

import           Telos.Agent.Config

import           Test.Hspec

spec :: Spec
spec = do
  describe "AgentConfig" $ do
    describe "defaultAgentConfig" $ do
      it "has maxIterations of 20" $ do
        (defaultAgentConfig ^. acMaxIterations) `shouldBe` 20

      it "has no system prompt by default" $ do
        (defaultAgentConfig ^. acSystemPrompt) `shouldBe` Nothing

      it "has default model gpt-4" $ do
        (defaultAgentConfig ^. acModel) `shouldBe` "gpt-4"

      it "has no MCP servers by default" $ do
        (defaultAgentConfig ^. acMCPServers) `shouldBe` []

  describe "MCPServerConfig" $ do
    it "can be constructed with all fields" $ do
      let cfg
            = makeMCPServerConfig "test-server" "/usr/bin/test" [ "--arg1", "--arg2" ]
            & mscEnv .~ [ ( "KEY", "VALUE" ) ]
      (cfg ^. mscName) `shouldBe` "test-server"
      (cfg ^. mscCommand) `shouldBe` "/usr/bin/test"
      (cfg ^. mscArgs) `shouldBe` [ "--arg1", "--arg2" ]
      (cfg ^. mscEnv) `shouldBe` [ ( "KEY", "VALUE" ) ]
