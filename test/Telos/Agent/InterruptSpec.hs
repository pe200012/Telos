module Telos.Agent.InterruptSpec ( spec ) where

import           Control.Concurrent.MVar ( isEmptyMVar )

import           Lens.Micro              ( (^.) )

import           Relude

import           Telos.Agent.Config      ( defaultAgentConfig )
import           Telos.Agent.Context     ( ctxInterrupt, newAgentContext )
import           Telos.Agent.Interrupt

import           Test.Hspec

spec :: Spec
spec = do
  describe "Interrupt handling" $ do
    describe "checkInterrupted" $ do
      it "returns False initially" $ do
        ctx <- newAgentContext defaultAgentConfig
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` False

      it "returns True after signalInterrupt" $ do
        ctx <- newAgentContext defaultAgentConfig
        signalInterrupt ctx
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` True

    describe "signalInterrupt" $ do
      it "fills the interrupt MVar" $ do
        ctx <- newAgentContext defaultAgentConfig
        empty1 <- isEmptyMVar (ctx ^. ctxInterrupt)
        empty1 `shouldBe` True
        signalInterrupt ctx
        empty2 <- isEmptyMVar (ctx ^. ctxInterrupt)
        empty2 `shouldBe` False

      it "is idempotent (multiple signals don't block)" $ do
        ctx <- newAgentContext defaultAgentConfig
        signalInterrupt ctx
        signalInterrupt ctx
        signalInterrupt ctx
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` True

    describe "clearInterrupt" $ do
      it "clears the interrupt signal" $ do
        ctx <- newAgentContext defaultAgentConfig
        signalInterrupt ctx
        interrupted1 <- checkInterrupted ctx
        interrupted1 `shouldBe` True
        clearInterrupt ctx
        interrupted2 <- checkInterrupted ctx
        interrupted2 `shouldBe` False

      it "is safe to call when not interrupted" $ do
        ctx <- newAgentContext defaultAgentConfig
        clearInterrupt ctx
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` False

    describe "withInterruptHandler" $ do
      it "runs action normally without interrupt" $ do
        ctx <- newAgentContext defaultAgentConfig
        result <- withInterruptHandler ctx $ pure (42 :: Int)
        result `shouldBe` 42

      it "restores handler after action completes" $ do
        ctx <- newAgentContext defaultAgentConfig
        _ <- withInterruptHandler ctx $ pure ()
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` False

    describe "installInterruptHandler / removeInterruptHandler" $ do
      it "can install and remove handler" $ do
        ctx <- newAgentContext defaultAgentConfig
        handler <- installInterruptHandler ctx
        removeInterruptHandler handler
        interrupted <- checkInterrupted ctx
        interrupted `shouldBe` False
