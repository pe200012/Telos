{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.Agent.Loop
  ( AgentResult(..)
  , runAgentLoop
  , runAgentLoopStreaming
  , agentStep
  , agentStepStreaming
  , executeToolCalls
  , mcpToolToCoreTool
  ) where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Polysemy                  ( Embed, Members, Sem, embed )

import           Telos.Agent.Config        ( AgentConfig(..) )
import           Telos.Agent.Context       ( AgentContext(..)
                                           , addMessage
                                           , getHistory
                                           , getIterationCount
                                           , getTools
                                           , incrementIteration
                                           , resetIteration
                                           )
import           Telos.Agent.Interrupt     ( checkInterrupted )
import           Telos.Agent.Streaming     ( StreamConsumeResult(..), consumeStreamWithInterrupt )
import qualified Telos.Core.Types          as Core
import           Telos.Effect.LLM          ( LLM, chat, chatStream )
import           Telos.Effect.Logger       ( Logger, logDebug, logInfo, logWarn )
import           Telos.Effect.MCP          ( ContentItem(..), MCP, ToolResult(..), callTool )
import           Telos.Effect.StreamOutput ( StreamOutput, flushOutput, outputNewline )
import qualified Telos.MCP.Types           as MCP

data AgentResult
  = AgentResponse Text
  | AgentInterrupted Text
  | AgentMaxIterations Text
  | AgentError Text
  deriving stock ( Eq, Show )

-- | Run agent loop with non-streaming LLM calls
runAgentLoop
  :: Members '[ LLM, MCP, Logger, Embed IO ] r => AgentContext -> Text -> Sem r AgentResult
runAgentLoop ctx userInput = do
  embed $ resetIteration ctx
  embed $ addMessage ctx (Core.UserMessage userInput)
  logInfo
    $ "User: "
    <> T.take 100 userInput
    <> if T.length userInput > 100
      then "..."
      else ""
  loop ctx
  where
    loop c = do
      interrupted <- embed $ checkInterrupted c
      if interrupted
        then do
          logInfo "Agent interrupted by user"
          history <- embed $ getHistory c
          let partialResponse = extractLastAssistantContent history
          pure $ AgentInterrupted partialResponse
        else do
          iteration <- embed $ getIterationCount c
          let maxIter = acMaxIterations (ctxConfig c)
          if iteration >= maxIter
            then do
              logWarn $ "Max iterations reached: " <> T.pack (show maxIter)
              history <- embed $ getHistory c
              let partialResponse = extractLastAssistantContent history
              pure $ AgentMaxIterations partialResponse
            else do
              result <- agentStep c
              case result of
                Left done -> pure done
                Right ()  -> loop c

-- | Run agent loop with streaming LLM calls
runAgentLoopStreaming :: Members '[ LLM, MCP, Logger, StreamOutput, Embed IO ] r
                      => AgentContext
                      -> Text
                      -> Sem r AgentResult
runAgentLoopStreaming ctx userInput = do
  embed $ resetIteration ctx
  embed $ addMessage ctx (Core.UserMessage userInput)
  logInfo
    $ "User: "
    <> T.take 100 userInput
    <> if T.length userInput > 100
      then "..."
      else ""
  loop ctx
  where
    loop c = do
      interrupted <- embed $ checkInterrupted c
      if interrupted
        then do
          logInfo "Agent interrupted by user"
          history <- embed $ getHistory c
          let partialResponse = extractLastAssistantContent history
          pure $ AgentInterrupted partialResponse
        else do
          iteration <- embed $ getIterationCount c
          let maxIter = acMaxIterations (ctxConfig c)
          if iteration >= maxIter
            then do
              logWarn $ "Max iterations reached: " <> T.pack (show maxIter)
              history <- embed $ getHistory c
              let partialResponse = extractLastAssistantContent history
              pure $ AgentMaxIterations partialResponse
            else do
              result <- agentStepStreaming c
              case result of
                Left done -> pure done
                Right ()  -> loop c

-- | Single non-streaming agent step
agentStep
  :: Members '[ LLM, MCP, Logger, Embed IO ] r => AgentContext -> Sem r (Either AgentResult ())
agentStep ctx = do
  iter <- embed $ incrementIteration ctx
  logDebug $ "Agent step " <> T.pack (show iter)

  history <- embed $ getHistory ctx
  tools <- embed $ getTools ctx

  let config       = ctxConfig ctx
      systemPrompt = acSystemPrompt config
      messages     = case systemPrompt of
        Nothing -> history
        Just sp -> Core.SystemMessage sp : history

  logDebug $ "Calling LLM with " <> T.pack (show $ length messages) <> " messages"

  response <- chat messages tools

  case response of
    Left err           -> do
      logWarn $ "LLM error: " <> T.pack (show err)
      pure $ Left $ AgentError $ T.pack (show err)

    Right assistantMsg -> do
      embed $ addMessage ctx (Core.AssistantMsg assistantMsg)
      handleAssistantMessage ctx assistantMsg

-- | Single streaming agent step
agentStepStreaming :: Members '[ LLM, MCP, Logger, StreamOutput, Embed IO ] r
                   => AgentContext
                   -> Sem r (Either AgentResult ())
agentStepStreaming ctx = do
  iter <- embed $ incrementIteration ctx
  logDebug $ "Agent step (streaming) " <> T.pack (show iter)

  history <- embed $ getHistory ctx
  tools <- embed $ getTools ctx

  let config       = ctxConfig ctx
      systemPrompt = acSystemPrompt config
      messages     = case systemPrompt of
        Nothing -> history
        Just sp -> Core.SystemMessage sp : history

  logDebug $ "Calling LLM (streaming) with " <> T.pack (show $ length messages) <> " messages"

  -- Get the streaming conduit
  conduit <- chatStream messages tools

  -- Consume stream with interrupt checking and real-time output
  let onEvent = streamEventHandler
  result <- embed $ consumeStreamWithInterrupt (ctxInterrupt ctx) onEvent conduit

  -- Output newline after stream ends
  outputNewline
  flushOutput

  case result of
    StreamConsumeFailed err -> do
      logWarn $ "Stream error: " <> err
      pure $ Left $ AgentError err

    StreamConsumeInterrupted partial -> do
      logInfo "Stream interrupted by user"
      let content = Core.pmContentSoFar partial
      pure $ Left $ AgentInterrupted content

    StreamConsumeCompleted assistantMsg -> do
      embed $ addMessage ctx (Core.AssistantMsg assistantMsg)
      handleAssistantMessage ctx assistantMsg

-- | Handle stream events for real-time output
streamEventHandler :: Core.StreamEvent -> IO ()
streamEventHandler = \case
  Core.ContentDelta text -> do
    TIO.putStr text
    -- Note: flushing handled by StreamOutput effect
  Core.ToolCallStart _ _ name -> do
    TIO.putStrLn $ "\n[Tool: " <> name <> "]"
  Core.ToolCallDelta _ _ -> pure ()  -- Arguments are not shown in real-time
  Core.Ping -> pure ()

-- | Handle assistant message (shared between streaming and non-streaming)
handleAssistantMessage :: Members '[ MCP, Logger, Embed IO ] r
                       => AgentContext
                       -> Core.AssistantMessage
                       -> Sem r (Either AgentResult ())
handleAssistantMessage ctx assistantMsg = do
  let toolCalls = Core.amToolCalls assistantMsg
      content   = fromMaybe "" (Core.amContent assistantMsg)

  if not (null toolCalls)
    then do
      logInfo $ "Assistant requested " <> T.pack (show $ length toolCalls) <> " tool calls"
      toolResults <- executeToolCalls ctx toolCalls
      mapM_ (embed . addMessage ctx) toolResults
      pure $ Right ()
    else do
      logInfo
        $ "Assistant: "
        <> T.take 100 content
        <> if T.length content > 100
          then "..."
          else ""
      pure $ Left $ AgentResponse content

executeToolCalls :: Members '[ MCP, Logger, Embed IO ] r
                 => AgentContext
                 -> [ Core.ToolCall ]
                 -> Sem r [ Core.Message ]
executeToolCalls _ctx toolCalls = do
  forM toolCalls $ \tc -> do
    let toolName = Core.tcName tc
        toolArgs = Core.tcArguments tc
        toolId   = Core.tcId tc

    logDebug $ "Executing tool: " <> toolName

    result <- callTool toolName toolArgs

    case result of
      Left err         -> do
        logWarn $ "Tool error: " <> T.pack (show err)
        pure $ Core.ToolResultMessage toolId toolName (T.pack $ show err) True

      Right toolResult -> do
        let content = formatToolResult toolResult
            isError = trIsError toolResult
        logDebug $ "Tool result: " <> T.take 200 content
        pure $ Core.ToolResultMessage toolId toolName content isError

formatToolResult :: ToolResult -> Text
formatToolResult result = T.intercalate "\n" $ map formatContentItem (trContent result)
  where
    formatContentItem :: ContentItem -> Text
    formatContentItem = \case
      TextContent t -> t
      ImageContent mime _ -> "[Image: " <> mime <> "]"
      EmbeddedResource uri txt -> "[Resource: " <> uri <> "]\n" <> txt

extractLastAssistantContent :: [ Core.Message ] -> Text
extractLastAssistantContent = go ""
  where
    go acc [] = acc
    go _ (Core.AssistantMsg am : rest) = go (fromMaybe "" $ Core.amContent am) rest
    go acc (_ : rest) = go acc rest

mcpToolToCoreTool :: Text -> MCP.ToolInfo -> Core.Tool
mcpToolToCoreTool serverName ti
  = Core.Tool { Core.toolName        = serverName <> "/" <> MCP.tiName ti
              , Core.toolDescription = MCP.tiDescription ti
              , Core.toolInputSchema = MCP.tiInputSchema ti
              }
