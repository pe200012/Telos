{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telos.Agent.LoopSpec ( spec ) where

import           Test.Hspec

import           Data.Aeson          ( object, (.=) )
import           Lens.Micro          ( (^.), (.~) )

import           Polysemy
import           Polysemy.Error

import           Telos.Agent.Config  ( defaultAgentConfig, acMaxIterations, makeAgentConfig )
import           Telos.Agent.Context ( newAgentContext, getHistory )
import           Telos.Agent.Loop
import           Telos.Core.Error    ( AppError(..), LLMError(..), MCPError(..) )
import           Telos.Core.Types
import           Telos.Effect.LLM    ( LLM(..) )
import           Telos.Effect.Logger ( Logger(..) )
import           Telos.Effect.MCP    ( MCP(..), ContentItem(..), ToolResult, makeToolResult )
import           Telos.MCP.Types     ( makeToolInfo, tiDescription )

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
  GetProviderInfo -> pure $ makeProviderInfo "mock" "mock"

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
        llmRef <- newIORef $ Right $ makeAssistantMessage (Just "Hello back!") []
        mcpRef <- newIORef $ Right $ makeToolResult []
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Hello")
        result `shouldBe` Right (AgentResponse "Hello back!")

      it "adds user message to history" $ do
        ctx <- newAgentContext defaultAgentConfig
        llmRef <- newIORef $ Right $ makeAssistantMessage (Just "Response") []
        mcpRef <- newIORef $ Right $ makeToolResult []
        
        _ <- runTest llmRef mcpRef (runAgentLoop ctx "Test message")
        history <- getHistory ctx
        length history `shouldBe` 2
        case viaNonEmpty head history of
          Just h  -> h `shouldBe` UserMessage "Test message"
          Nothing -> expectationFailure "History should not be empty"

      it "handles LLM errors" $ do
        ctx <- newAgentContext defaultAgentConfig
        llmRef <- newIORef $ Left $ LLMAuthError "auth failed"
        mcpRef <- newIORef $ Right $ makeToolResult []
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Hello")
        case result of
          Right (AgentError _) -> pure ()
          _ -> expectationFailure "Expected AgentError"

      it "respects max iterations" $ do
        let config = makeAgentConfig "gpt-4" & acMaxIterations .~ 2
        ctx <- newAgentContext config
        llmRef <- newIORef $ Right $ makeAssistantMessage Nothing 
          [makeToolCall "id" "test/tool" (object ["arg" .= ("val" :: Text)])]
        mcpRef <- newIORef $ Right $ makeToolResult [TextContent "result"]
        
        result <- runTest llmRef mcpRef (runAgentLoop ctx "Loop forever")
        case result of
          Right (AgentMaxIterations _) -> pure ()
          _ -> expectationFailure "Expected AgentMaxIterations"

    describe "executeToolCalls" $ do
      it "executes tool calls and returns results" $ do
        ctx <- newAgentContext defaultAgentConfig
        let toolCalls = [makeToolCall "tc1" "test/tool" (object ["x" .= (1 :: Int)])]
        llmRef <- newIORef $ Right $ makeAssistantMessage Nothing []
        mcpRef <- newIORef $ Right $ makeToolResult [TextContent "Tool executed"]
        
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
        let toolCalls = [makeToolCall "tc1" "failing/tool" (object [])]
        llmRef <- newIORef $ Right $ makeAssistantMessage Nothing []
        mcpRef <- newIORef $ Left $ MCPToolExecutionFailed "Tool failed"
        
        result <- runTest llmRef mcpRef (executeToolCalls ctx toolCalls)
        case result of
          Right [ToolResultMessage _ _ _ isErr] -> isErr `shouldBe` True
          _ -> expectationFailure "Expected ToolResultMessage with error"

    describe "mcpToolToCoreTool" $ do
      it "converts MCP ToolInfo to Core Tool with server prefix" $ do
        let mcpTool = makeToolInfo "read_file" (object ["type" .= ("object" :: Text)])
                        & tiDescription .~ Just "Reads a file"
            coreTool = mcpToolToCoreTool "filesystem" mcpTool
        
        (coreTool ^. toolName) `shouldBe` "filesystem/read_file"
        (coreTool ^. toolDescription) `shouldBe` Just "Reads a file"

      it "handles tools without description" $ do
        let mcpTool = makeToolInfo "simple" (object [])
            coreTool = mcpToolToCoreTool "server" mcpTool
        
        (coreTool ^. toolName) `shouldBe` "server/simple"
        (coreTool ^. toolDescription) `shouldBe` Nothing
