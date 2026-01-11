{-# LANGUAGE OverloadedStrings #-}

module Telos.Effect.StreamOutputSpec ( spec ) where

import           Polysemy

import           Relude

import           Telos.Effect.StreamOutput
import           Telos.Effect.StreamOutput.IO ( runStreamOutputSilent )

import           Test.Hspec

spec :: Spec
spec = do
  describe "StreamOutput effect" $ do
    describe "runStreamOutputSilent" $ do
      it "discards all output" $ do
        result <- runM $ runStreamOutputSilent $ do
          outputChunk "Hello"
          outputChunk " World"
          outputToolStart "test_tool"
          outputToolEnd "test_tool"
          outputNewline
          flushOutput
          pure ("done" :: Text)
        result `shouldBe` "done"

    describe "runStreamOutputCollect" $ do
      it "collects content chunks" $ do
        result <- runStreamOutputCollect $ do
          outputChunk "Hello"
          outputChunk " World"
        result `shouldBe` [ "Hello", " World" ]

      it "collects tool start events" $ do
        result <- runStreamOutputCollect $ do
          outputToolStart "my_tool"
        result `shouldBe` [ "[TOOL_START:my_tool]" ]

      it "collects tool end events" $ do
        result <- runStreamOutputCollect $ do
          outputToolEnd "my_tool"
        result `shouldBe` [ "[TOOL_END:my_tool]" ]

      it "collects newlines" $ do
        result <- runStreamOutputCollect $ do
          outputChunk "line1"
          outputNewline
          outputChunk "line2"
        result `shouldBe` [ "line1", "[NEWLINE]", "line2" ]

      it "ignores flush" $ do
        result <- runStreamOutputCollect $ do
          outputChunk "test"
          flushOutput
        result `shouldBe` [ "test" ]

-- | Test interpreter that collects output as a list of Text
runStreamOutputCollect :: Sem '[ StreamOutput, Embed IO ] a -> IO [ Text ]
runStreamOutputCollect action = do
  ref <- newIORef []
  _ <- runM $ runStreamOutputToRef ref action
  reverse <$> readIORef ref

-- | Interpreter that collects output to an IORef
runStreamOutputToRef
  :: Member (Embed IO) r => IORef [ Text ] -> Sem (StreamOutput ': r) a -> Sem r a
runStreamOutputToRef ref = interpret $ \case
  OutputChunk text     -> embed @IO $ modifyIORef' ref (text :)
  OutputToolStart name -> embed @IO $ modifyIORef' ref (("[TOOL_START:" <> name <> "]") :)
  OutputToolEnd name   -> embed @IO $ modifyIORef' ref (("[TOOL_END:" <> name <> "]") :)
  OutputNewline        -> embed @IO $ modifyIORef' ref ("[NEWLINE]" :)
  FlushOutput          -> pure ()
