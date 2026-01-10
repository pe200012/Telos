{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Interpreter ( runLLMWithCopilot ) where

import           Conduit                  ( (.|), ConduitT, awaitForever, yield )

import           Lens.Micro               ( (.~), (^.) )

import           Polysemy                 ( Embed, InterpreterFor, Member, embed, interpret )

import           Telos.Core.Error         ( LLMError(..) )
import           Telos.Core.Types         ( AssistantMessage
                                          , makePartialMessage
                                          , makeProviderInfo
                                          , piMaxTokens
                                          , StreamEvent(..)
                                          , StreamResult(..)
                                          )
import           Telos.Effect.LLM         ( LLM(..) )
import           Telos.LLM.Copilot.Client ( ChatResponse(..)
                                          , CopilotClient(..)
                                          , Delta(..)
                                          , ToolCallChunk(..)
                                          , chChoices
                                          , chDelta
                                          , chMessage
                                          , clConfig
                                          , ccModel
                                          , ccMaxTokens
                                          , dContent
                                          , dToolCalls
                                          , fcArguments
                                          , fcName
                                          , tccFunction
                                          , tccId
                                          , tccIndex
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
    $ makeProviderInfo "GitHub Copilot" (client ^. clConfig . ccModel)
      & piMaxTokens .~ (client ^. clConfig . ccMaxTokens)

-- | Extract assistant message from chat response
extractAssistantMessage :: ChatResponse -> Maybe AssistantMessage
extractAssistantMessage resp = do
  choice <- listToMaybe (resp ^. chChoices)
  choice ^. chMessage

-- | Convert ChatResponse stream to StreamEvent stream.
-- The conduit yields StreamEvents and returns a dummy StreamResult.
-- The actual result should be computed by the consumer (Agent/Streaming.hs)
-- by accumulating events, since the SSE stream doesn't provide a final message.
streamToEvents :: ConduitT () ChatResponse IO () -> ConduitT () StreamEvent IO StreamResult
streamToEvents source = do
  source .| convertEvents
  -- Return a placeholder - the consumer will build the final message from accumulated events
  pure $ StreamInterrupted (makePartialMessage "" [])
  where
    convertEvents :: ConduitT ChatResponse StreamEvent IO ()
    convertEvents = awaitForever $ \resp -> case chatResponseToStreamEvents resp of
      []     -> pure ()
      events -> mapM_ yield events

-- | Convert a single ChatResponse to zero or more StreamEvents.
-- A single response chunk can contain both content and tool call updates.
chatResponseToStreamEvents :: ChatResponse -> [ StreamEvent ]
chatResponseToStreamEvents resp = case listToMaybe (resp ^. chChoices) of
  Nothing     -> []
  Just choice -> case choice ^. chDelta of
    Nothing    -> []
    Just delta -> contentEvent delta ++ toolCallEvents delta

-- | Extract content delta event if present
contentEvent :: Delta -> [ StreamEvent ]
contentEvent delta = case delta ^. dContent of
  Just content
    | content /= "" -> [ ContentDelta content ]
  _ -> []

-- | Extract tool call events from delta
toolCallEvents :: Delta -> [ StreamEvent ]
toolCallEvents delta = case delta ^. dToolCalls of
  Nothing  -> []
  Just tcs -> concatMap toolCallChunkToEvents tcs

-- | Convert a tool call chunk to stream events.
-- First chunk for an index contains id and name (ToolCallStart).
-- Subsequent chunks contain argument fragments (ToolCallDelta).
toolCallChunkToEvents :: ToolCallChunk -> [ StreamEvent ]
toolCallChunkToEvents tc = case tc ^. tccFunction of
  Nothing -> []
  Just fc -> let
      startEvent = case ( tc ^. tccId, fc ^. fcName ) of
        ( Just tcId, Just name ) -> [ ToolCallStart (tc ^. tccIndex) tcId name ]
        _ -> []
      deltaEvent = case fc ^. fcArguments of
        Just args
          | args /= "" -> [ ToolCallDelta (tc ^. tccIndex) args ]
        _         -> []
    in 
      startEvent ++ deltaEvent

-- | Create an empty conduit that immediately returns a result
emptyConduit :: a -> ConduitT i o m a
emptyConduit = pure
