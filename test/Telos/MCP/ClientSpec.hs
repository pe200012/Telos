module Telos.MCP.ClientSpec ( spec ) where

import           Relude

import           Telos.MCP.Client
import           Telos.MCP.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "MCPConnection" $ do
    describe "connectToServer" $ do
      it "fails gracefully for non-existent command" $ do
        let config = makeServerConfig "test" "/nonexistent/mcp/server" []
        result <- connectToServer config
        case result of
          Left _     -> pure ()
          Right conn -> do
            disconnectFromServer conn
            expectationFailure "Should have failed for non-existent command"

      it "fails gracefully when server doesn't speak MCP protocol" $ do
        let config = makeServerConfig "test-cat" "cat" []
        result <- connectToServer config
        case result of
          Left _     -> pure ()  -- Expected: cat doesn't speak MCP
          Right conn -> do
            disconnectFromServer conn
            expectationFailure "Should have failed - cat doesn't speak MCP"
