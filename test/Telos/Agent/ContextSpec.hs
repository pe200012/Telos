module Telos.Agent.ContextSpec ( spec ) where

import           Control.Concurrent.MVar ( isEmptyMVar )
import           Data.Aeson              ( object )
import           Test.Hspec

import           Telos.Agent.Config      ( defaultAgentConfig )
import Telos.Agent.Context
import Telos.Core.Types ( Message(..), Tool(..) )

spec :: Spec
spec = do
  describe "AgentContext" $ do
    describe "newAgentContext" $ do
      it "creates context with empty history" $ do
        ctx <- newAgentContext defaultAgentConfig
        history <- getHistory ctx
        history `shouldBe` []

      it "creates context with empty tools" $ do
        ctx <- newAgentContext defaultAgentConfig
        tools <- getTools ctx
        tools `shouldBe` []

      it "creates context with iteration count 0" $ do
        ctx <- newAgentContext defaultAgentConfig
        count <- getIterationCount ctx
        count `shouldBe` 0

      it "creates context with empty interrupt MVar" $ do
        ctx <- newAgentContext defaultAgentConfig
        isEmpty <- isEmptyMVar (ctxInterrupt ctx)
        isEmpty `shouldBe` True

    describe "addMessage" $ do
      it "adds message to history" $ do
        ctx <- newAgentContext defaultAgentConfig
        addMessage ctx (UserMessage "Hello")
        history <- getHistory ctx
        history `shouldBe` [UserMessage "Hello"]

      it "appends messages in order" $ do
        ctx <- newAgentContext defaultAgentConfig
        addMessage ctx (UserMessage "First")
        addMessage ctx (UserMessage "Second")
        addMessage ctx (UserMessage "Third")
        history <- getHistory ctx
        history `shouldBe` [UserMessage "First", UserMessage "Second", UserMessage "Third"]

    describe "setHistory" $ do
      it "replaces entire history" $ do
        ctx <- newAgentContext defaultAgentConfig
        addMessage ctx (UserMessage "Old")
        setHistory ctx [UserMessage "New1", UserMessage "New2"]
        history <- getHistory ctx
        history `shouldBe` [UserMessage "New1", UserMessage "New2"]

    describe "clearHistory" $ do
      it "removes all messages" $ do
        ctx <- newAgentContext defaultAgentConfig
        addMessage ctx (UserMessage "To be cleared")
        clearHistory ctx
        history <- getHistory ctx
        history `shouldBe` []

    describe "registerTools" $ do
      it "registers tools" $ do
        ctx <- newAgentContext defaultAgentConfig
        let testTool = Tool "test/tool" (Just "A test tool") (object [])
        registerTools ctx [testTool]
        tools <- getTools ctx
        tools `shouldBe` [testTool]

      it "replaces existing tools" $ do
        ctx <- newAgentContext defaultAgentConfig
        let tool1 = Tool "tool1" Nothing (object [])
            tool2 = Tool "tool2" Nothing (object [])
        registerTools ctx [tool1]
        registerTools ctx [tool2]
        tools <- getTools ctx
        tools `shouldBe` [tool2]

    describe "iteration count" $ do
      it "incrementIteration increases count" $ do
        ctx <- newAgentContext defaultAgentConfig
        n1 <- incrementIteration ctx
        n1 `shouldBe` 1
        n2 <- incrementIteration ctx
        n2 `shouldBe` 2
        n3 <- incrementIteration ctx
        n3 `shouldBe` 3

      it "resetIteration sets count to 0" $ do
        ctx <- newAgentContext defaultAgentConfig
        _ <- incrementIteration ctx
        _ <- incrementIteration ctx
        resetIteration ctx
        count <- getIterationCount ctx
        count `shouldBe` 0

    describe "interrupt MVar" $ do
      it "can be signaled" $ do
        ctx <- newAgentContext defaultAgentConfig
        _ <- tryPutMVar (ctxInterrupt ctx) ()
        isEmpty <- isEmptyMVar (ctxInterrupt ctx)
        isEmpty `shouldBe` False
