{-# LANGUAGE OverloadedStrings #-}

module Telos.Agent.StreamingSpec ( spec ) where

import qualified Data.IntMap.Strict    as IntMap

import           Lens.Micro            ( (^.) )

import           Telos.Agent.Streaming
import           Telos.Core.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "StreamAccumulator" $ do
    describe "emptyAccumulator" $ do
      it "starts with empty content" $ do
        (emptyAccumulator ^. saContent) `shouldBe` ""

      it "starts with no tool calls" $ do
        IntMap.null (emptyAccumulator ^. saToolCalls) `shouldBe` True

    describe "accumulate" $ do
      it "accumulates content deltas" $ do
        let acc1 = accumulate emptyAccumulator (ContentDelta "Hello ")
            acc2 = accumulate acc1 (ContentDelta "World")
        (acc2 ^. saContent) `shouldBe` "Hello World"

      it "handles empty content deltas" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "")
        (acc ^. saContent) `shouldBe` ""

      it "starts tool calls with ToolCallStart" $ do
        let acc = accumulate emptyAccumulator (ToolCallStart 0 "call-123" "get_weather")
        IntMap.size (acc ^. saToolCalls) `shouldBe` 1
        case IntMap.lookup 0 (acc ^. saToolCalls) of
          Nothing  -> expectationFailure "Tool call not found"
          Just ptc -> do
            (ptc ^. ptcId) `shouldBe` Just "call-123"
            (ptc ^. ptcName) `shouldBe` Just "get_weather"
            (ptc ^. ptcArgumentsSoFar) `shouldBe` ""

      it "accumulates tool call arguments with ToolCallDelta" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-123" "get_weather")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"loc")
            acc3 = accumulate acc2 (ToolCallDelta 0 "ation\":\"NYC\"}")
        case IntMap.lookup 0 (acc3 ^. saToolCalls) of
          Nothing  -> expectationFailure "Tool call not found"
          Just ptc -> (ptc ^. ptcArgumentsSoFar) `shouldBe` "{\"location\":\"NYC\"}"

      it "handles multiple tool calls by index" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool_a")
            acc2 = accumulate acc1 (ToolCallStart 1 "call-2" "tool_b")
            acc3 = accumulate acc2 (ToolCallDelta 0 "{\"a\":1}")
            acc4 = accumulate acc3 (ToolCallDelta 1 "{\"b\":2}")
        IntMap.size (acc4 ^. saToolCalls) `shouldBe` 2
        case IntMap.lookup 0 (acc4 ^. saToolCalls) of
          Nothing  -> expectationFailure "Tool call 0 not found"
          Just ptc -> (ptc ^. ptcArgumentsSoFar) `shouldBe` "{\"a\":1}"
        case IntMap.lookup 1 (acc4 ^. saToolCalls) of
          Nothing  -> expectationFailure "Tool call 1 not found"
          Just ptc -> (ptc ^. ptcArgumentsSoFar) `shouldBe` "{\"b\":2}"

      it "ignores Ping events" $ do
        let acc1 = accumulate emptyAccumulator (ContentDelta "test")
            acc2 = accumulate acc1 Ping
        (acc2 ^. saContent) `shouldBe` "test"
        IntMap.size (acc2 ^. saToolCalls) `shouldBe` 0

    describe "finalizeAccumulator" $ do
      it "creates PartialMessage with accumulated content" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "Hello")
            pm  = finalizeAccumulator acc
        (pm ^. pmContentSoFar) `shouldBe` "Hello"
        (pm ^. pmToolCallsSoFar) `shouldBe` []

      it "includes partial tool calls" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "id-1" "tool")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"x\":")
            pm   = finalizeAccumulator acc2
        length (pm ^. pmToolCallsSoFar) `shouldBe` 1
        let ptc = fromMaybe (error "Empty tool calls") $ viaNonEmpty head (pm ^. pmToolCallsSoFar)
        (ptc ^. ptcArgumentsSoFar) `shouldBe` "{\"x\":"

    describe "accumulatorToAssistantMessage" $ do
      it "converts empty accumulator to empty message" $ do
        case accumulatorToAssistantMessage emptyAccumulator of
          Left err  -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            (msg ^. amContent) `shouldBe` Nothing
            (msg ^. amToolCalls) `shouldBe` []

      it "converts content-only accumulator" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "Hello world")
        case accumulatorToAssistantMessage acc of
          Left err  -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            (msg ^. amContent) `shouldBe` Just "Hello world"
            (msg ^. amToolCalls) `shouldBe` []

      it "converts tool call with valid JSON arguments" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "get_weather")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"location\":\"NYC\"}")
        case accumulatorToAssistantMessage acc2 of
          Left err  -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            (msg ^. amContent) `shouldBe` Nothing
            length (msg ^. amToolCalls) `shouldBe` 1
            let tc = fromMaybe (error "Empty tool calls") $ viaNonEmpty head (msg ^. amToolCalls)
            (tc ^. tcId) `shouldBe` "call-1"
            (tc ^. tcName) `shouldBe` "get_weather"

      it "fails on invalid JSON arguments" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool")
            acc2 = accumulate acc1 (ToolCallDelta 0 "not valid json")
        case accumulatorToAssistantMessage acc2 of
          Left _  -> pure ()  -- Expected failure
          Right _ -> expectationFailure "Should have failed on invalid JSON"

      it "handles empty arguments as empty object" $ do
        let acc = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool")
        case accumulatorToAssistantMessage acc of
          Left err  -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> length (msg ^. amToolCalls) `shouldBe` 1
