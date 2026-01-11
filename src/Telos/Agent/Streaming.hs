{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Streaming
  ( StreamAccumulator
  , saContent
  , saToolCalls
  , emptyAccumulator
  , accumulate
  , finalizeAccumulator
  , accumulatorToAssistantMessage
  , consumeStreamWithInterrupt
  , StreamConsumeResult(..)
  ) where

import           Conduit

import           Control.Concurrent.MVar ( isEmptyMVar )

import           Data.Aeson              ( Value(..), eitherDecodeStrict' )
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE

import           Lens.Micro              ( (%~), (^.) )
import           Lens.Micro.TH           ( makeLenses )

import           Relude

import           Telos.Core.Types

data StreamAccumulator
  = StreamAccumulator { _saContent :: !Text, _saToolCalls :: !(IntMap PartialToolCall) }
  deriving ( Show, Eq )

makeLenses ''StreamAccumulator

emptyAccumulator :: StreamAccumulator
emptyAccumulator = StreamAccumulator { _saContent = "", _saToolCalls = IntMap.empty }

accumulate :: StreamAccumulator -> StreamEvent -> StreamAccumulator
accumulate !acc event = case event of
  ContentDelta txt -> acc & saContent %~ (<> txt)

  ToolCallStart idx toolId tName -> let
      ptc = makePartialToolCall toolId tName ""
    in 
      acc & saToolCalls %~ IntMap.insert idx ptc

  ToolCallDelta idx argChunk -> let
      updateArgs ptc = ptc & ptcArgumentsSoFar %~ (<> argChunk)
    in 
      acc & saToolCalls %~ IntMap.adjust updateArgs idx

  Ping -> acc

finalizeAccumulator :: StreamAccumulator -> PartialMessage
finalizeAccumulator acc = makePartialMessage (acc ^. saContent) (IntMap.elems (acc ^. saToolCalls))

accumulatorToAssistantMessage :: StreamAccumulator -> Either Text AssistantMessage
accumulatorToAssistantMessage acc = do
  toolCalls <- traverse partialToToolCall (IntMap.elems (acc ^. saToolCalls))
  let content
        = if T.null (acc ^. saContent)
          then Nothing
          else Just (acc ^. saContent)
  Right $ makeAssistantMessage content toolCalls
  where
    partialToToolCall :: PartialToolCall -> Either Text ToolCall
    partialToToolCall ptc = do
      toolId <- maybe (Left "Missing tool call ID") Right (ptc ^. ptcId)
      tName <- maybe (Left "Missing tool call name") Right (ptc ^. ptcName)
      args <- parseArguments (ptc ^. ptcArgumentsSoFar)
      Right (makeToolCall toolId tName args)

    parseArguments :: Text -> Either Text Value
    parseArguments "" = Right (Object mempty)
    parseArguments t  = case eitherDecodeStrict' (TE.encodeUtf8 t) of
      Left err -> Left $ "Failed to parse tool arguments: " <> toText err
      Right v  -> Right v

data StreamConsumeResult
  = StreamConsumeCompleted !AssistantMessage
  | StreamConsumeInterrupted !PartialMessage
  | StreamConsumeFailed !Text
  deriving ( Show, Eq )

consumeStreamWithInterrupt :: MVar ()
                           -> (StreamEvent -> IO ())
                           -> ConduitT () StreamEvent IO StreamResult
                           -> IO StreamConsumeResult
consumeStreamWithInterrupt interruptVar onEvent source = do
  accRef <- newIORef emptyAccumulator
  _streamResult <- runConduit $ fuseUpstream source (processEvents accRef)
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
          interrupted <- liftIO $ not <$> isEmptyMVar interruptVar
          unless interrupted $ do
            mevent <- await
            case mevent of
              Nothing    -> pass
              Just event -> do
                liftIO $ do
                  modifyIORef' accRef (`accumulate` event)
                  onEvent event
                interruptedAfter <- liftIO $ not <$> isEmptyMVar interruptVar
                unless interruptedAfter loop
