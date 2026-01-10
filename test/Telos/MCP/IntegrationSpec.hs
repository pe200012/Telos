module Telos.MCP.IntegrationSpec (spec) where

import           Data.Aeson              ( object, (.=) )
import           Test.Hspec
import Telos.MCP.Client
import Telos.MCP.ServerManager
import Telos.MCP.Types

spec :: Spec
spec = do
  describe "MCP Integration with real server" $ do
    describe "Client" $ do
      it "connects to filesystem MCP server and lists tools" $ do
        let config = ServerConfig
              { scName = "filesystem"
              , scCommand = "npx"
              , scArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
              , scWorkDir = Nothing
              , scEnv = Nothing
              }
        result <- connectToServer config
        case result of
          Left err -> expectationFailure $ "Failed to connect: " <> show err
          Right conn -> do
            toolsResult <- listTools conn
            disconnectFromServer conn
            case toolsResult of
              Left err -> expectationFailure $ "Failed to list tools: " <> show err
              Right ltr -> do
                let tools = ltrTools ltr
                length tools `shouldSatisfy` (> 0)
                let toolNames = map tiName tools
                toolNames `shouldSatisfy` \names -> 
                  any (`elem` names) ["read_file", "write_file", "list_directory"]

      it "calls read_file tool successfully" $ do
        let config = ServerConfig
              { scName = "filesystem"
              , scCommand = "npx"
              , scArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
              , scWorkDir = Nothing
              , scEnv = Nothing
              }
        result <- connectToServer config
        case result of
          Left err -> expectationFailure $ "Failed to connect: " <> show err
          Right conn -> do
            writeFile "/tmp/mcp_test_file.txt" "Hello MCP!"
            let args = object ["path" .= ("/tmp/mcp_test_file.txt" :: String)]
            callResult <- callTool conn "read_file" (Just args)
            disconnectFromServer conn
            case callResult of
              Left err -> expectationFailure $ "Failed to call tool: " <> show err
              Right ctr -> do
                ctrIsError ctr `shouldNotBe` Just True
                ctrContent ctr `shouldBe` [TextPart "Hello MCP!"]

    describe "ServerManager" $ do
      it "manages multiple tool aggregation" $ do
        mgr <- newServerManager
        let config = ServerConfig
              { scName = "filesystem"
              , scCommand = "npx"
              , scArgs = ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
              , scWorkDir = Nothing
              , scEnv = Nothing
              }
        addResult <- addServer mgr config
        case addResult of
          Left err -> expectationFailure $ "Failed to add server: " <> show err
          Right _conn -> do
            toolsResult <- aggregateTools mgr
            shutdownAll mgr
            case toolsResult of
              Left err -> expectationFailure $ "Failed to aggregate tools: " <> show err
              Right tools -> do
                length tools `shouldSatisfy` (> 0)
