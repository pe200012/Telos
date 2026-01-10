{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Telos.Agent.StreamingSpec (spec) where

import Test.Hspec
import Data.IntMap.Strict qualified as IntMap

import Telos.Agent.Streaming
import Telos.Core.Types

spec :: Spec
spec = do
  describe "StreamAccumulator" $ do
    describe "emptyAccumulator" $ do
      it "starts with empty content" $ do
        saContent emptyAccumulator `shouldBe` ""
      
      it "starts with no tool calls" $ do
        IntMap.null (saToolCalls emptyAccumulator) `shouldBe` True

    describe "accumulate" $ do
      it "accumulates content deltas" $ do
        let acc1 = accumulate emptyAccumulator (ContentDelta "Hello ")
            acc2 = accumulate acc1 (ContentDelta "World")
        saContent acc2 `shouldBe` "Hello World"
      
      it "handles empty content deltas" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "")
        saContent acc `shouldBe` ""
      
      it "starts tool calls with ToolCallStart" $ do
        let acc = accumulate emptyAccumulator (ToolCallStart 0 "call-123" "get_weather")
        IntMap.size (saToolCalls acc) `shouldBe` 1
        case IntMap.lookup 0 (saToolCalls acc) of
          Nothing -> expectationFailure "Tool call not found"
          Just ptc -> do
            ptcId ptc `shouldBe` Just "call-123"
            ptcName ptc `shouldBe` Just "get_weather"
            ptcArgumentsSoFar ptc `shouldBe` ""
      
      it "accumulates tool call arguments with ToolCallDelta" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-123" "get_weather")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"loc")
            acc3 = accumulate acc2 (ToolCallDelta 0 "ation\":\"NYC\"}")
        case IntMap.lookup 0 (saToolCalls acc3) of
          Nothing -> expectationFailure "Tool call not found"
          Just ptc -> ptcArgumentsSoFar ptc `shouldBe` "{\"location\":\"NYC\"}"
      
      it "handles multiple tool calls by index" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool_a")
            acc2 = accumulate acc1 (ToolCallStart 1 "call-2" "tool_b")
            acc3 = accumulate acc2 (ToolCallDelta 0 "{\"a\":1}")
            acc4 = accumulate acc3 (ToolCallDelta 1 "{\"b\":2}")
        IntMap.size (saToolCalls acc4) `shouldBe` 2
        case IntMap.lookup 0 (saToolCalls acc4) of
          Nothing -> expectationFailure "Tool call 0 not found"
          Just ptc -> ptcArgumentsSoFar ptc `shouldBe` "{\"a\":1}"
        case IntMap.lookup 1 (saToolCalls acc4) of
          Nothing -> expectationFailure "Tool call 1 not found"
          Just ptc -> ptcArgumentsSoFar ptc `shouldBe` "{\"b\":2}"
      
      it "ignores Ping events" $ do
        let acc1 = accumulate emptyAccumulator (ContentDelta "test")
            acc2 = accumulate acc1 Ping
        saContent acc2 `shouldBe` "test"
        IntMap.size (saToolCalls acc2) `shouldBe` 0

    describe "finalizeAccumulator" $ do
      it "creates PartialMessage with accumulated content" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "Hello")
            pm = finalizeAccumulator acc
        pmContentSoFar pm `shouldBe` "Hello"
        pmToolCallsSoFar pm `shouldBe` []
      
      it "includes partial tool calls" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "id-1" "tool")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"x\":")
            pm = finalizeAccumulator acc2
        length (pmToolCallsSoFar pm) `shouldBe` 1
        let ptc = fromMaybe (error "Empty tool calls") $ viaNonEmpty head (pmToolCallsSoFar pm)
        ptcArgumentsSoFar ptc `shouldBe` "{\"x\":"

    describe "accumulatorToAssistantMessage" $ do
      it "converts empty accumulator to empty message" $ do
        case accumulatorToAssistantMessage emptyAccumulator of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            amContent msg `shouldBe` Nothing
            amToolCalls msg `shouldBe` []
      
      it "converts content-only accumulator" $ do
        let acc = accumulate emptyAccumulator (ContentDelta "Hello world")
        case accumulatorToAssistantMessage acc of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            amContent msg `shouldBe` Just "Hello world"
            amToolCalls msg `shouldBe` []
      
      it "converts tool call with valid JSON arguments" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "get_weather")
            acc2 = accumulate acc1 (ToolCallDelta 0 "{\"location\":\"NYC\"}")
        case accumulatorToAssistantMessage acc2 of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> do
            amContent msg `shouldBe` Nothing
            length (amToolCalls msg) `shouldBe` 1
            let tc = fromMaybe (error "Empty tool calls") $ viaNonEmpty head (amToolCalls msg)
            tcId tc `shouldBe` "call-1"
            tcName tc `shouldBe` "get_weather"
      
      it "fails on invalid JSON arguments" $ do
        let acc1 = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool")
            acc2 = accumulate acc1 (ToolCallDelta 0 "not valid json")
        case accumulatorToAssistantMessage acc2 of
          Left _ -> pure ()  -- Expected failure
          Right _ -> expectationFailure "Should have failed on invalid JSON"
      
      it "handles empty arguments as empty object" $ do
        let acc = accumulate emptyAccumulator (ToolCallStart 0 "call-1" "tool")
        case accumulatorToAssistantMessage acc of
          Left err -> expectationFailure $ "Unexpected error: " ++ show err
          Right msg -> length (amToolCalls msg) `shouldBe` 1
