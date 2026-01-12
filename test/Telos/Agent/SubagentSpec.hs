{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.Agent.SubagentSpec ( spec ) where

import           Control.Concurrent.MVar ( isEmptyMVar )

import           Data.Aeson              ( object )

import qualified Data.Text               as T

import           Control.Lens              ( (^.) )

import           Polysemy                ( runM )

import           Relude

import           Telos.Agent.Config      ( acMaxIterations, defaultAgentConfig )
import           Telos.Agent.Context
import           Telos.Agent.Subagent
import           Telos.Core.Types        ( Message(..), makeTool )

import           Test.Hspec

spec :: Spec
spec = do
  describe "SubagentConfig" $ do
    describe "defaultSubagentConfig" $ do
      it "creates config with given prompt" $ do
        let cfg = defaultSubagentConfig "Test task"
        _sacPrompt cfg `shouldBe` "Test task"

      it "sets default max iterations to 10" $ do
        let cfg = defaultSubagentConfig "Test"
        _sacMaxIterations cfg `shouldBe` 10

      it "sets default max depth to 3" $ do
        let cfg = defaultSubagentConfig "Test"
        _sacMaxDepth cfg `shouldBe` 3

      it "sets current depth to 0" $ do
        let cfg = defaultSubagentConfig "Test"
        _sacCurrentDepth cfg `shouldBe` 0

  describe "SubagentResult" $ do
    it "SubagentSuccess holds response text" $ do
      let result = SubagentSuccess "Task completed"
      case result of
        SubagentSuccess txt -> txt `shouldBe` "Task completed"
        _ -> expectationFailure "Expected SubagentSuccess"

    it "SubagentError holds error message" $ do
      let result = SubagentError "Something went wrong"
      case result of
        SubagentError err -> err `shouldBe` "Something went wrong"
        _ -> expectationFailure "Expected SubagentError"

  describe "createSubagentContext" $ do
    it "creates child context with empty history" $ do
      parentCtx <- newAgentContext defaultAgentConfig
      addMessage parentCtx (UserMessage "Parent message")

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      childHistory <- getHistory childCtx
      childHistory `shouldBe` []

    it "parent history is not affected by child" $ do
      parentCtx <- newAgentContext defaultAgentConfig
      addMessage parentCtx (UserMessage "Parent message")

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      addMessage childCtx (UserMessage "Child message")

      parentHistory <- getHistory parentCtx
      parentHistory `shouldBe` [UserMessage "Parent message"]

    it "creates child context with fresh iteration counter" $ do
      parentCtx <- newAgentContext defaultAgentConfig
      _ <- incrementIteration parentCtx
      _ <- incrementIteration parentCtx

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      childIter <- getIterationCount childCtx
      childIter `shouldBe` 0

    it "shares tools between parent and child" $ do
      parentCtx <- newAgentContext defaultAgentConfig
      let testTool = makeTool "test/tool" (object [])
      registerTools parentCtx [testTool]

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      childTools <- getTools childCtx
      childTools `shouldBe` [testTool]

    it "child tool registration affects parent (shared)" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      let testTool = makeTool "child/tool" (object [])
      registerTools childCtx [testTool]

      parentTools <- getTools parentCtx
      parentTools `shouldBe` [testTool]

    it "shares interrupt MVar between parent and child" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = defaultSubagentConfig "Child task"
      childCtx <- createSubagentContext parentCtx cfg

      -- Signal interrupt on parent
      _ <- tryPutMVar (parentCtx ^. ctxInterrupt) ()

      -- Child should see it
      childEmpty <- isEmptyMVar (childCtx ^. ctxInterrupt)
      childEmpty `shouldBe` False

    it "copies config with adjusted max iterations" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = SubagentConfig
            { _sacPrompt        = "Task"
            , _sacMaxIterations = 5
            , _sacMaxDepth      = 3
            , _sacCurrentDepth  = 0
            }
      childCtx <- createSubagentContext parentCtx cfg

      childConfig <- readTVarIO (childCtx ^. ctxConfig)
      (childConfig ^. acMaxIterations) `shouldBe` 5

    it "child config changes do not affect parent" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = SubagentConfig
            { _sacPrompt        = "Task"
            , _sacMaxIterations = 5
            , _sacMaxDepth      = 3
            , _sacCurrentDepth  = 0
            }
      _childCtx <- createSubagentContext parentCtx cfg

      -- Verify parent config unchanged
      parentConfig <- readTVarIO (parentCtx ^. ctxConfig)
      (parentConfig ^. acMaxIterations) `shouldBe` 100  -- default is 100

  describe "depth limiting" $ do
    it "runSubagent returns error when depth exceeded" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = SubagentConfig
            { _sacPrompt        = "Task"
            , _sacMaxIterations = 10
            , _sacMaxDepth      = 3
            , _sacCurrentDepth  = 3  -- Already at max
            }

      -- Mock runner that should not be called
      let mockRunner _ _ = pure $ SubagentSuccess "Should not reach here"

      result <- runM $ runSubagent mockRunner parentCtx cfg
      case result of
        SubagentError err -> err `shouldSatisfy` T.isInfixOf "depth exceeded"
        _ -> expectationFailure "Expected SubagentError for depth exceeded"

    it "runSubagent proceeds when depth is within limit" $ do
      parentCtx <- newAgentContext defaultAgentConfig

      let cfg = SubagentConfig
            { _sacPrompt        = "Task"
            , _sacMaxIterations = 10
            , _sacMaxDepth      = 3
            , _sacCurrentDepth  = 2  -- One below max
            }

      -- Mock runner that returns success
      let mockRunner _ prompt = pure $ SubagentSuccess ("Executed: " <> prompt)

      result <- runM $ runSubagent mockRunner parentCtx cfg
      case result of
        SubagentSuccess txt -> txt `shouldBe` "Executed: Task"
        _ -> expectationFailure "Expected SubagentSuccess"
