{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telos.Agent.LoopSpec ( spec ) where

import           Test.Hspec

import           Data.Aeson          ( object, (.=) )

import           Polysemy
import Polysemy.Error

import Telos.Agent.Config ( AgentConfig(..), defaultAgentConfig )
import Telos.Agent.Context ( newAgentContext, getHistory )
import Telos.Agent.Loop
import Telos.Core.Error ( AppError(..), LLMError(..), MCPError(..) )
import Telos.Core.Types
import Telos.Effect.LLM ( LLM(..) )
import Telos.Effect.Logger ( Logger(..) )
import Telos.Effect.MCP ( MCP(..), ContentItem(..), ToolResult(..) )
import Telos.MCP.Types ( ToolInfo(..) )

runNoopLogger :: InterpreterFor Logger r
runNoopLogger = interpret $ \case
  Log' _ _ -> pure ()

runMockLLM 
  :: Member (Embed IO) r 
  => IORef (Either LLMError AssistantMessage) 
  -> InterpreterFor LLM r
runMockLLM ref = interpret $ \case
  Chat _ _ -> embed @IO $ readIORef ref
  ChatStream _ _ -> pure $ error "Not implemented in mock"
  GetProviderInfo -> pure $ ProviderInfo "mock" "mock" True Nothing

runMockMCP :: Member (Embed IO) r => IORef (Either MCPError ToolResult) -> InterpreterFor MCP r
runMockMCP ref = interpret $ \case
  ListTools -> pure []
  CallTool _ _ -> embed @IO $ readIORef ref
  ListResources -> pure []
  ReadResource _ -> pure $ Left $ MCPToolExecutionFailed "not impl"

runTest
  :: IORef (Either LLMError AssistantMessage)
  -> IORef (Either MCPError ToolResult)
  -> Sem '[LLM, MCP, Logger, Error AppError, Embed IO] a
  -> IO (Either AppError a)
runTest llmRef mcpRef action =
  runM
    $ runError @AppError
    $ runNoopLogger
    $ runMockMCP mcpRef
    $ runMockLLM llmRef action

spec :: Spec
spec = do
  describe "AgentLoop" $ do
    describe "runAgentLoop" $ do
      it "returns simple text response" $ do
        ctx <- newAgentContext defaultAgentConfig
        llmRef <- newIORef $ Right $ AssistantMessage (Just "Hello back!") []
        mcpRef <- newIORef $ Right $ ToolResult [] False
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Hello")
        result `shouldBe` Right (AgentResponse "Hello back!")

      it "adds user message to history" $ do
        ctx <- newAgentContext defaultAgentConfig
        llmRef <- newIORef $ Right $ AssistantMessage (Just "Response") []
        mcpRef <- newIORef $ Right $ ToolResult [] False
        
        _ <- runTest llmRef mcpRef (runAgentLoop ctx "Test message")
        history <- getHistory ctx
        length history `shouldBe` 2
        case viaNonEmpty head history of
          Just h  -> h `shouldBe` UserMessage "Test message"
          Nothing -> expectationFailure "History should not be empty"

      it "handles LLM errors" $ do
        ctx <- newAgentContext defaultAgentConfig
        llmRef <- newIORef $ Left $ LLMAuthError "auth failed"
        mcpRef <- newIORef $ Right $ ToolResult [] False
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Hello")
        case result of
          Right (AgentError _) -> pure ()
          _ -> expectationFailure "Expected AgentError"

      it "respects max iterations" $ do
        let config = defaultAgentConfig { acMaxIterations = 2 }
        ctx <- newAgentContext config
        llmRef <- newIORef $ Right $ AssistantMessage Nothing 
          [ToolCall "id" "test/tool" (object ["arg" .= ("val" :: Text)])]
        mcpRef <- newIORef $ Right $ ToolResult [TextContent "result"] False
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Loop forever")
        case result of
          Right (AgentMaxIterations _) -> pure ()
          _ -> expectationFailure "Expected AgentMaxIterations"

    describe "executeToolCalls" $ do
      it "executes tool calls and returns results" $ do
        ctx <- newAgentContext defaultAgentConfig
        let toolCalls = [ToolCall "tc1" "test/tool" (object ["x" .= (1 :: Int)])]
        llmRef <- newIORef $ Right $ AssistantMessage Nothing []
        mcpRef <- newIORef $ Right $ ToolResult [TextContent "Tool executed"] False
        
        result <- runTest llmRef mcpRef (executeToolCalls ctx toolCalls)
        case result of
          Right [ToolResultMessage tid name content isErr] -> do
            tid `shouldBe` "tc1"
            name `shouldBe` "test/tool"
            content `shouldBe` "Tool executed"
            isErr `shouldBe` False
          _ -> expectationFailure "Expected single ToolResultMessage"

      it "handles tool errors" $ do
        ctx <- newAgentContext defaultAgentConfig
        let toolCalls = [ToolCall "tc1" "failing/tool" (object [])]
        llmRef <- newIORef $ Right $ AssistantMessage Nothing []
        mcpRef <- newIORef $ Left $ MCPToolExecutionFailed "Tool failed"
        
        result <- runTest llmRef mcpRef (executeToolCalls ctx toolCalls)
        case result of
          Right [ToolResultMessage _ _ _ isErr] -> isErr `shouldBe` True
          _ -> expectationFailure "Expected ToolResultMessage with error"

    describe "mcpToolToCoreTool" $ do
      it "converts MCP ToolInfo to Core Tool with server prefix" $ do
        let mcpTool = ToolInfo
              { tiName = "read_file"
              , tiDescription = Just "Reads a file"
              , tiInputSchema = object ["type" .= ("object" :: Text)]
              }
            coreTool = mcpToolToCoreTool "filesystem" mcpTool
        
        toolName coreTool `shouldBe` "filesystem/read_file"
        toolDescription coreTool `shouldBe` Just "Reads a file"

      it "handles tools without description" $ do
        let mcpTool = ToolInfo
              { tiName = "simple"
              , tiDescription = Nothing
              , tiInputSchema = object []
              }
            coreTool = mcpToolToCoreTool "server" mcpTool
        
        toolName coreTool `shouldBe` "server/simple"
        toolDescription coreTool `shouldBe` Nothing
