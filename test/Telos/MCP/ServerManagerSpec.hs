module Telos.MCP.ServerManagerSpec (spec) where

import Data.Maybe (isNothing)
import Test.Hspec

import Telos.MCP.ServerManager
import Telos.MCP.Types

spec :: Spec
spec = do
  describe "ServerManager" $ do
    describe "newServerManager" $ do
      it "creates an empty manager" $ do
        mgr <- newServerManager
        conns <- getAllConnections mgr
        length conns `shouldBe` 0

    describe "getConnection" $ do
      it "returns Nothing for non-existent server" $ do
        mgr <- newServerManager
        result <- getConnection mgr "nonexistent"
        isNothing result `shouldBe` True

    describe "removeServer" $ do
      it "does nothing for non-existent server" $ do
        mgr <- newServerManager
        removeServer mgr "nonexistent"
        conns <- getAllConnections mgr
        length conns `shouldBe` 0

    describe "shutdownAll" $ do
      it "works on empty manager" $ do
        mgr <- newServerManager
        shutdownAll mgr
        conns <- getAllConnections mgr
        length conns `shouldBe` 0

    describe "aggregateTools" $ do
      it "returns empty list for empty manager" $ do
        mgr <- newServerManager
        result <- aggregateTools mgr
        case result of
          Left _ -> expectationFailure "Should not fail on empty manager"
          Right tools -> length tools `shouldBe` 0

    describe "findToolServer" $ do
      it "returns Nothing when no servers" $ do
        mgr <- newServerManager
        result <- findToolServer mgr "someTool"
        isNothing result `shouldBe` True
