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

import           Lens.Micro                ( (.~), (^.), _last, non )

import           Polysemy                  ( Embed, Members, Sem, embed )

import           Telos.Agent.Config        ( acMaxIterations, acSystemPrompt )
import           Telos.Agent.Context       ( AgentContext
                                           , addMessage
                                           , ctxConfig
                                           , ctxInterrupt
                                           , ctxToolContext
                                           , getHistory
                                           , getIterationCount
                                           , getTools
                                           , incrementIteration
                                           , resetIteration
                                           )
import           Telos.Agent.Interrupt     ( checkInterrupted )
import           Telos.Agent.Streaming     ( StreamConsumeResult(..), consumeStreamWithInterrupt )
import qualified Telos.Core.Types          as Core
import           Telos.Core.Types          ( _AssistantMsg )
import           Telos.Effect.LLM          ( LLM, chat, chatStream )
import           Telos.Effect.Logger       ( Logger, logDebug, logInfo, logWarn )
import           Telos.Effect.MCP          ( ContentItem(..)
                                           , MCP
                                           , ToolResult
                                           , callTool
                                           , trContent
                                           , trIsError
                                           )
import           Telos.Effect.StreamOutput ( StreamOutput, flushOutput, outputNewline )
import qualified Telos.MCP.Types           as MCP
import           Telos.Tool.Registry       ( executeBuiltinTool )
import qualified Telos.Tool.Types          as ToolTypes

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
          let maxIter = c ^. ctxConfig . acMaxIterations
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
          let maxIter = c ^. ctxConfig . acMaxIterations
          if iteration >= maxIter
            then do
              logWarn $ "Max iterations reached: " <> T.pack (show maxIter)
              history <- embed $ getHistory c
              let partialResponse = extractLastAssistantContent history
              pure $ AgentMaxIterations partialResponse
            else agentStepStreaming c >>= \case
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

  let systemPrompt = ctx ^. ctxConfig . acSystemPrompt
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

  let systemPrompt = ctx ^. ctxConfig . acSystemPrompt
      messages     = case systemPrompt of
        Nothing -> history
        Just sp -> Core.SystemMessage sp : history

  logDebug $ "Calling LLM (streaming) with " <> T.pack (show $ length messages) <> " messages"

  -- Get the streaming conduit
  conduit <- chatStream messages tools

  -- Consume stream with interrupt checking and real-time output
  let onEvent = streamEventHandler
  result <- embed $ consumeStreamWithInterrupt (ctx ^. ctxInterrupt) onEvent conduit

  -- Output newline after stream ends
  outputNewline
  flushOutput

  case result of
    StreamConsumeFailed err -> do
      logWarn $ "Stream error: " <> err
      pure $ Left $ AgentError err

    StreamConsumeInterrupted partial -> do
      logInfo "Stream interrupted by user"
      let content = partial ^. Core.pmContentSoFar
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
  let toolCalls = assistantMsg ^. Core.amToolCalls
      content   = assistantMsg ^. Core.amContent . non ""

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
executeToolCalls ctx toolCalls = do
  forM toolCalls $ \tc -> do
    let tName    = tc ^. Core.tcName
        toolArgs = tc ^. Core.tcArguments
        toolId   = tc ^. Core.tcId
        toolCtx  = ctx ^. ctxToolContext

    logDebug $ "Executing tool: " <> tName

    -- Try builtin tool first
    mBuiltinResult <- embed $ executeBuiltinTool toolCtx tName toolArgs

    case mBuiltinResult of
      Just builtinResult -> do
        let content = builtinResult ^. ToolTypes.trOutput
            isError = not (builtinResult ^. ToolTypes.trSuccess)
        logDebug $ "Builtin tool result: " <> content
        pure $ Core.ToolResultMessage toolId tName content isError

      Nothing -> do
        -- Fallback to MCP tool
        result <- callTool tName toolArgs

        case result of
          Left err         -> do
            logWarn $ "Tool error: " <> T.pack (show err)
            pure $ Core.ToolResultMessage toolId tName (T.pack $ show err) True

          Right toolResult -> do
            let content = formatToolResult toolResult
                isError = toolResult ^. trIsError
            logDebug $ "Tool result: " <> content
            pure $ Core.ToolResultMessage toolId tName content isError

formatToolResult :: ToolResult -> Text
formatToolResult result = T.intercalate "\n" $ map formatContentItem (result ^. trContent)
  where
    formatContentItem :: ContentItem -> Text
    formatContentItem = \case
      TextContent t -> t
      ImageContent mime _ -> "[Image: " <> mime <> "]"
      EmbeddedResource uri txt -> "[Resource: " <> uri <> "]\n" <> txt

-- | Extract the last assistant message content from history
-- Uses Prism to cleanly filter for AssistantMsg constructors
extractLastAssistantContent :: [ Core.Message ] -> Text
extractLastAssistantContent msgs = msgs ^. _last . _AssistantMsg . Core.amContent . non ""

mcpToolToCoreTool :: Text -> MCP.ToolInfo -> Core.Tool
mcpToolToCoreTool serverName ti
  = Core.makeTool (serverName <> "/" <> (ti ^. MCP.tiName)) (ti ^. MCP.tiInputSchema)
  & Core.toolDescription .~ (ti ^. MCP.tiDescription)

