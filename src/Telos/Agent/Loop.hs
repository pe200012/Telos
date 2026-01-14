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

import           Control.Lens              ( (%~), (.~), (^.), (^?), _last, non )

import           Data.Aeson                ( Value )
import           Data.Aeson.Lens           ( _Integer, _String, key )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Polysemy                  ( Embed, Members, Sem, embed )

import           Relude

import           Telos.Agent.Config        ( acMaxIterations, acPromptConfig, acPruneConfig )
import           Telos.Agent.Context       ( AgentContext(..)
                                           , addMessage
                                           , ctxConfig
                                           , ctxInterrupt
                                           , ctxToolContext
                                           , getHistory
                                           , getIterationCount
                                           , getPruneState
                                           , getTools
                                           , incrementIteration
                                           , modifyPruneState
                                           , resetIteration
                                           )
import           Telos.Agent.Interrupt     ( checkInterrupted )
import           Telos.Agent.Streaming     ( StreamConsumeResult(..), consumeStreamWithInterrupt )
import           Telos.Agent.Subagent      ( SubagentConfig(..), SubagentResult(..), runSubagent )
import           Telos.Context.Strategy    ( computeParamKey, extractFilePath, runStrategies )
import           Telos.Context.Transform   ( injectPrunableList
                                           , transformMessages
                                           , updateToolCache
                                           )
import           Telos.Context.Types       ( pcEnabled, psCurrentTurn )
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
import           Telos.Prompt.Builder      ( buildSystemPrompt )
import           Telos.Tool.Registry       ( executeBuiltinTool, isAgentTool, taskToolName )
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
  -- Increment turn counter for DCP tracking
  embed $ modifyPruneState ctx $ \ps -> ps & psCurrentTurn %~ (+ 1)
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
          cfg <- embed $ readTVarIO @IO (c ^. ctxConfig)
          let maxIter = cfg ^. acMaxIterations
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
          cfg <- embed $ readTVarIO @IO (c ^. ctxConfig)
          let maxIter = cfg ^. acMaxIterations
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

  config <- embed $ readTVarIO @IO (ctx ^. ctxConfig)
  let mPromptConfig = config ^. acPromptConfig
      pruneConfig   = config ^. acPruneConfig

  -- Build base messages with system prompt
  baseMessages <- case mPromptConfig of
    Nothing  -> pure history
    Just cfg -> do
      sysPrompt <- embed $ buildSystemPrompt cfg
      pure $ Core.SystemMessage sysPrompt : history

  -- Apply DCP if enabled
  messages <- if pruneConfig ^. pcEnabled
    then do
      pruneState <- embed $ getPruneState ctx
      let pruneState' = runStrategies pruneConfig pruneState
      embed $ modifyPruneState ctx (const pruneState')
      let transformed = transformMessages pruneState' baseMessages
      pure $ injectPrunableList pruneConfig pruneState' transformed
    else pure baseMessages

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

  config <- embed $ readTVarIO @IO (ctx ^. ctxConfig)
  let mPromptConfig = config ^. acPromptConfig
      pruneConfig   = config ^. acPruneConfig

  -- Build base messages with system prompt
  baseMessages <- case mPromptConfig of
    Nothing  -> pure history
    Just cfg -> do
      sysPrompt <- embed $ buildSystemPrompt cfg
      pure $ Core.SystemMessage sysPrompt : history

  -- Apply DCP if enabled
  messages <- if pruneConfig ^. pcEnabled
    then do
      pruneState <- embed $ getPruneState ctx
      let pruneState' = runStrategies pruneConfig pruneState
      embed $ modifyPruneState ctx (const pruneState')
      let transformed = transformMessages pruneState' baseMessages
      pure $ injectPrunableList pruneConfig pruneState' transformed
    else pure baseMessages

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
handleAssistantMessage :: Members '[ LLM, MCP, Logger, Embed IO ] r
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

executeToolCalls :: Members '[ LLM, MCP, Logger, Embed IO ] r
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

    -- Check if this is an agent-aware tool (like 'task')
    resultMsg <- if isAgentTool tName
      then executeAgentTool ctx tc
      else do
        -- Try builtin tool first, with streaming callback for streaming tools
        let streamCallback chunk = do
              -- Output chunk to terminal in real-time
              TIO.putStr chunk
              hFlush stdout

        mBuiltinResult <- embed $ executeBuiltinTool streamCallback toolCtx tName toolArgs

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

    -- Update tool cache for DCP tracking
    let ( resultContent, isError ) = case resultMsg of
          Core.ToolResultMessage _ _ c e -> ( c, e )
          _ -> ( "", False )
        paramKey = computeParamKey tName toolArgs
        filePath = extractFilePath tName toolArgs
    embed $ modifyPruneState ctx $ \ps
      -> updateToolCache ps tName paramKey isError filePath resultContent

    pure resultMsg

-- | Execute an agent-aware tool (like 'task')
executeAgentTool :: Members '[ LLM, MCP, Logger, Embed IO ] r
                 => AgentContext
                 -> Core.ToolCall
                 -> Sem r Core.Message
executeAgentTool ctx tc = do
  let tName    = tc ^. Core.tcName
      toolArgs = tc ^. Core.tcArguments
      toolId   = tc ^. Core.tcId

  if tName == taskToolName
    then executeTaskTool ctx toolId toolArgs
    else do
      -- Unknown agent tool
      logWarn $ "Unknown agent tool: " <> tName
      pure $ Core.ToolResultMessage toolId tName ("Unknown agent tool: " <> tName) True

-- | Execute the 'task' tool to spawn a subagent
executeTaskTool :: Members '[ LLM, MCP, Logger, Embed IO ] r
                => AgentContext
                -> Text  -- ^ Tool call ID
                -> Value -- ^ Arguments
                -> Sem r Core.Message
executeTaskTool ctx toolId args = do
  -- Parse arguments
  let mPrompt = args ^? key "prompt" . _String
      maxIter = fromMaybe 10 $ args ^? key "max_iterations" . _Integer
      mDesc   = args ^? key "description" . _String

  case mPrompt of
    Nothing     -> do
      logWarn "Task tool missing required 'prompt' parameter"
      pure $ Core.ToolResultMessage toolId taskToolName "Missing required parameter: prompt" True

    Just prompt -> do
      logInfo $ "Spawning subagent: " <> mDesc ^. non (T.take 50 prompt)

      let cfg
            = SubagentConfig { _sacPrompt        = prompt
                             , _sacMaxIterations = fromIntegral maxIter
                             , _sacMaxDepth      = 3
                             , _sacCurrentDepth  = 0  -- TODO: track depth through parent context
                             }

      -- Run subagent using the non-streaming loop
      result <- runSubagent subagentRunner ctx cfg

      case result of
        SubagentSuccess response -> do
          logInfo "Subagent completed successfully"
          pure $ Core.ToolResultMessage toolId taskToolName response False

        SubagentError err        -> do
          logWarn $ "Subagent error: " <> err
          pure $ Core.ToolResultMessage toolId taskToolName ("Subagent error: " <> err) True

        SubagentMaxIterations    -> do
          logWarn "Subagent hit max iterations"
          pure
            $ Core.ToolResultMessage toolId taskToolName "Subagent reached maximum iterations" True

        SubagentInterrupted      -> do
          logInfo "Subagent was interrupted"
          pure $ Core.ToolResultMessage toolId taskToolName "Subagent was interrupted" True

-- | Runner function for subagents (converts AgentResult to SubagentResult)
subagentRunner
  :: Members '[ LLM, MCP, Logger, Embed IO ] r => AgentContext -> Text -> Sem r SubagentResult
subagentRunner ctx prompt = do
  result <- runAgentLoop ctx prompt
  pure $ case result of
    AgentResponse txt    -> SubagentSuccess txt
    AgentInterrupted _   -> SubagentInterrupted
    AgentMaxIterations _ -> SubagentMaxIterations
    AgentError err       -> SubagentError err

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

