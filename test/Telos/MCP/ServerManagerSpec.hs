module Telos.MCP.ServerManagerSpec ( spec ) where

import           Telos.MCP.ServerManager

import           Test.Hspec

spec :: Spec
spec = do
  describe "ServerManager" $ do
    describe "newServerManager" $ do
      it "creates an empty manager" $ do
        mgr <- newServerManager
        statuses <- getServerStatus mgr
        length statuses `shouldBe` 0

    describe "getOrConnectServer" $ do
      it "returns error for non-registered server" $ do
        mgr <- newServerManager
        result <- getOrConnectServer mgr "nonexistent"
        isLeft result `shouldBe` True

    describe "getServerStatus" $ do
      it "returns empty list for empty manager" $ do
        mgr <- newServerManager
        statuses <- getServerStatus mgr
        length statuses `shouldBe` 0

    describe "shutdownAll" $ do
      it "works on empty manager" $ do
        mgr <- newServerManager
        shutdownAll mgr
        statuses <- getServerStatus mgr
        length statuses `shouldBe` 0

    describe "aggregateTools" $ do
      it "returns empty list for empty manager" $ do
        mgr <- newServerManager
        result <- aggregateTools mgr
        case result of
          Left _      -> expectationFailure "Should not fail on empty manager"
          Right tools -> length tools `shouldBe` 0

    describe "findToolServer" $ do
      it "returns Nothing when no servers" $ do
        mgr <- newServerManager
        result <- findToolServer mgr "someTool"
        isNothing result `shouldBe` True
