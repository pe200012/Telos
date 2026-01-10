{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Interpreter ( runLLMWithCopilot ) where

import           Conduit                  ( (.|), ConduitT, awaitForever, yield )

import           Data.Maybe               ( fromMaybe, listToMaybe )
import           Data.Text                ( Text )

import           Polysemy                 ( Embed, InterpreterFor, Member, Sem, embed, interpret )

import           Telos.Core.Error         ( LLMError(..) )
import           Telos.Core.Types         ( AssistantMessage
                                          , PartialMessage(..)
                                          , ProviderInfo(..)
                                          , StreamEvent(..)
                                          , StreamResult(..)
                                          )
import           Telos.Effect.LLM         ( LLM(..) )
import           Telos.LLM.Copilot.Client ( ChatResponse(..)
                                          , Choice(..)
                                          , CopilotClient(..)
                                          , CopilotConfig(..)
                                          , Delta(..)
                                          , FunctionChunk(..)
                                          , ToolCallChunk(..)
                                          , sendChatRequest
                                          , sendChatRequestStream
                                          )

-- | Run LLM effect with Copilot backend
runLLMWithCopilot :: Member (Embed IO) r => CopilotClient -> InterpreterFor LLM r
runLLMWithCopilot client = interpret $ \case
  Chat messages tools       -> do
    result <- embed $ sendChatRequest client messages tools
    pure $ case result of
      Left err   -> Left $ LLMNetworkError err
      Right resp -> case extractAssistantMessage resp of
        Nothing  -> Left $ LLMParseError "No assistant message in response"
        Just msg -> Right msg

  ChatStream messages tools -> do
    result <- embed $ sendChatRequestStream client messages tools
    pure $ case result of
      Left err     -> emptyConduit (StreamFailed err)
      Right source -> streamToEvents source

  GetProviderInfo           -> pure
    ProviderInfo { piName          = "GitHub Copilot"
                 , piModel         = ccModel (clConfig client)
                 , piSupportsTools = True
                 , piMaxTokens     = ccMaxTokens (clConfig client)
                 }

-- | Extract assistant message from chat response
extractAssistantMessage :: ChatResponse -> Maybe AssistantMessage
extractAssistantMessage resp = do
  choice <- listToMaybe (chChoices resp)
  chMessage choice

-- | Convert ChatResponse stream to StreamEvent stream.
-- The conduit yields StreamEvents and returns a dummy StreamResult.
-- The actual result should be computed by the consumer (Agent/Streaming.hs)
-- by accumulating events, since the SSE stream doesn't provide a final message.
streamToEvents :: ConduitT () ChatResponse IO () -> ConduitT () StreamEvent IO StreamResult
streamToEvents source = do
  source .| convertEvents
  -- Return a placeholder - the consumer will build the final message from accumulated events
  pure $ StreamInterrupted (PartialMessage "" [])
  where
    convertEvents :: ConduitT ChatResponse StreamEvent IO ()
    convertEvents = awaitForever $ \resp -> case chatResponseToStreamEvents resp of
      []     -> pure ()
      events -> mapM_ yield events

-- | Convert a single ChatResponse to zero or more StreamEvents.
-- A single response chunk can contain both content and tool call updates.
chatResponseToStreamEvents :: ChatResponse -> [ StreamEvent ]
chatResponseToStreamEvents resp = case listToMaybe (chChoices resp) of
  Nothing     -> []
  Just choice -> case chDelta choice of
    Nothing    -> []
    Just delta -> contentEvent delta ++ toolCallEvents delta

-- | Extract content delta event if present
contentEvent :: Delta -> [ StreamEvent ]
contentEvent delta = case dContent delta of
  Just content
    | content /= "" -> [ ContentDelta content ]
  _ -> []

-- | Extract tool call events from delta
toolCallEvents :: Delta -> [ StreamEvent ]
toolCallEvents delta = case dToolCalls delta of
  Nothing  -> []
  Just tcs -> concatMap toolCallChunkToEvents tcs

-- | Convert a tool call chunk to stream events.
-- First chunk for an index contains id and name (ToolCallStart).
-- Subsequent chunks contain argument fragments (ToolCallDelta).
toolCallChunkToEvents :: ToolCallChunk -> [ StreamEvent ]
toolCallChunkToEvents tc = case tccFunction tc of
  Nothing -> []
  Just fc -> let
      startEvent = case ( tccId tc, fcName fc ) of
        ( Just tcId, Just name ) -> [ ToolCallStart (tccIndex tc) tcId name ]
        _ -> []
      deltaEvent = case fcArguments fc of
        Just args
          | args /= "" -> [ ToolCallDelta (tccIndex tc) args ]
        _         -> []
    in 
      startEvent ++ deltaEvent

-- | Create an empty conduit that immediately returns a result
emptyConduit :: Monad m => a -> ConduitT i o m a
emptyConduit = pure
