{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Streaming response handling for the agent loop.
-- Handles accumulation of stream events and interrupt checking.
module Telos.Agent.Streaming
  ( -- * Accumulator
    StreamAccumulator(..)
  , emptyAccumulator
  , accumulate
  , finalizeAccumulator
  , accumulatorToAssistantMessage
    -- * Stream Processing
  , consumeStreamWithInterrupt
  , StreamConsumeResult(..)
  ) where

import           Conduit

import           Control.Concurrent.MVar ( MVar, isEmptyMVar )
import           Control.Monad           ( unless )

import           Data.Aeson              ( Value(..), eitherDecodeStrict' )
import           Data.IORef
import           Data.IntMap.Strict      ( IntMap )
import qualified Data.IntMap.Strict      as IntMap
import           Data.Text               ( Text )
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE

import           Telos.Core.Types

-- | Accumulator for building up a message from stream events.
data StreamAccumulator
  = StreamAccumulator { saContent   :: !Text
                        -- ^ Accumulated text content
                      , saToolCalls :: !(IntMap PartialToolCall)
                        -- ^ Tool calls indexed by their stream index
                      }
  deriving ( Show, Eq )

-- | Create an empty accumulator.
emptyAccumulator :: StreamAccumulator
emptyAccumulator = StreamAccumulator { saContent = "", saToolCalls = IntMap.empty }

-- | Accumulate a stream event into the accumulator.
accumulate :: StreamAccumulator -> StreamEvent -> StreamAccumulator
accumulate !acc event = case event of
  ContentDelta text -> acc { saContent = saContent acc <> text }

  ToolCallStart idx toolId toolName -> let
      ptc
        = PartialToolCall { ptcId = Just toolId, ptcName = Just toolName, ptcArgumentsSoFar = "" }
    in 
      acc { saToolCalls = IntMap.insert idx ptc (saToolCalls acc) }

  ToolCallDelta idx argChunk -> let
      updateArgs ptc = ptc { ptcArgumentsSoFar = ptcArgumentsSoFar ptc <> argChunk }
    in 
      acc { saToolCalls = IntMap.adjust updateArgs idx (saToolCalls acc) }

  Ping -> acc

-- | Finalize the accumulator into a PartialMessage.
finalizeAccumulator :: StreamAccumulator -> PartialMessage
finalizeAccumulator acc
  = PartialMessage
  { pmContentSoFar = saContent acc, pmToolCallsSoFar = IntMap.elems (saToolCalls acc) }

-- | Convert a completed accumulator to an AssistantMessage.
-- Returns Left with error message if tool call arguments fail to parse as JSON.
accumulatorToAssistantMessage :: StreamAccumulator -> Either Text AssistantMessage
accumulatorToAssistantMessage acc = do
  toolCalls <- traverse partialToToolCall (IntMap.elems (saToolCalls acc))
  let content
        = if T.null (saContent acc)
          then Nothing
          else Just (saContent acc)
  Right AssistantMessage { amContent = content, amToolCalls = toolCalls }
  where
    partialToToolCall :: PartialToolCall -> Either Text ToolCall
    partialToToolCall ptc = do
      toolId <- maybe (Left "Missing tool call ID") Right (ptcId ptc)
      toolName <- maybe (Left "Missing tool call name") Right (ptcName ptc)
      args <- parseArguments (ptcArgumentsSoFar ptc)
      Right ToolCall { tcId = toolId, tcName = toolName, tcArguments = args }

    parseArguments :: Text -> Either Text Value
    parseArguments "" = Right (Object mempty)
    parseArguments t  = case eitherDecodeStrict' (TE.encodeUtf8 t) of
      Left err -> Left $ "Failed to parse tool arguments: " <> T.pack err
      Right v  -> Right v

-- | Result of consuming a stream.
data StreamConsumeResult
  = StreamConsumeCompleted !AssistantMessage
    -- ^ Stream completed successfully
  | StreamConsumeInterrupted !PartialMessage
    -- ^ Stream was interrupted by user
  | StreamConsumeFailed !Text
    -- ^ Stream failed with error
  deriving ( Show, Eq )

-- | Consume a stream with interrupt checking.
-- Calls the event handler for each event (for real-time output).
-- Checks the interrupt MVar before processing each event.
consumeStreamWithInterrupt
  :: MVar ()                                    -- ^ Interrupt signal
  -> (StreamEvent -> IO ())                     -- ^ Event handler (for output)
  -> ConduitT () StreamEvent IO StreamResult    -- ^ Source conduit
  -> IO StreamConsumeResult
consumeStreamWithInterrupt interruptVar onEvent source = do
  accRef <- newIORef emptyAccumulator

  -- Run the conduit, consuming events and accumulating
  -- We use fuseBoth to run both the source and sink, getting both results
  _streamResult <- runConduit $ fuseUpstream source (processEvents accRef)

  -- Check if we were interrupted
  interrupted <- not <$> isEmptyMVar interruptVar

  acc <- readIORef accRef
  if interrupted
    then pure $ StreamConsumeInterrupted (finalizeAccumulator acc)
    else case accumulatorToAssistantMessage acc of
      Left err  -> pure $ StreamConsumeFailed err
      Right msg -> pure $ StreamConsumeCompleted msg
  where
    processEvents :: IORef StreamAccumulator -> ConduitT StreamEvent Void IO ()
    processEvents accRef = loop
      where
        loop = do
          -- Check for interrupt before awaiting
          interrupted <- liftIO $ not <$> isEmptyMVar interruptVar
          unless interrupted $ do
            mevent <- await
            case mevent of
              Nothing    -> pure ()  -- Stream ended
              Just event -> do
                -- Process the event
                liftIO $ do
                  modifyIORef' accRef (`accumulate` event)
                  onEvent event

                -- Check interrupt after processing
                interruptedAfter <- liftIO $ not <$> isEmptyMVar interruptVar
                unless interruptedAfter loop
