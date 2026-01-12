module Telos.Tool.BashSpec ( spec ) where

import qualified Data.Aeson       as Aeson
import qualified Data.Text        as T

import           Control.Lens       ( (^.) )

import           Relude

import           Telos.Tool.Bash  ( bashTool )
import           Telos.Tool.Types ( BuiltinTool(..)
                                  , ToolContext
                                  , ToolExecutorType(..)
                                  , ToolResult
                                  , newToolContext
                                  , trOutput
                                  , trSuccess
                                  )

import           Test.Hspec

-- | Run executor, handling both SimpleExecutor and StreamingExecutor
runExecutor :: BuiltinTool -> ToolContext -> Aeson.Value -> IO ToolResult
runExecutor bt ctx args = case _btExecutor bt of
  SimpleExecutor f    -> f ctx args
  StreamingExecutor f -> f (const $ pure ()) ctx args  -- Discard streaming output in tests
  _                   -> error "Not a supported executor"

spec :: Spec
spec = describe "BashTool" $ do
  describe "command execution" $ do
    it "executes simple echo command" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "command" Aeson..= ("echo hello" :: Text) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "hello"

    it "returns exit code in success flag" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "command" Aeson..= ("exit 1" :: Text) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` False

    it "captures stderr output" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "command" Aeson..= ("echo error >&2" :: Text) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "error"

  describe "workdir parameter" $ do
    it "executes command in specified directory" $ do
      ctx <- newToolContext
      let args
            = Aeson.object
              [ "command" Aeson..= ("pwd" :: Text), "workdir" Aeson..= ("/tmp" :: Text) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "/tmp"

    it "fails for non-existent directory" $ do
      ctx <- newToolContext
      let args
            = Aeson.object
              [ "command" Aeson..= ("pwd" :: Text)
              , "workdir" Aeson..= ("/nonexistent_dir_12345" :: Text)
              ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` False

  describe "timeout parameter" $ do
    it "times out long-running command" $ do
      ctx <- newToolContext
      let args
            = Aeson.object
              [ "command" Aeson..= ("sleep 10" :: Text), "timeout" Aeson..= (100 :: Int) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "timed out"

    it "completes fast command within timeout" $ do
      ctx <- newToolContext
      let args
            = Aeson.object
              [ "command" Aeson..= ("echo fast" :: Text), "timeout" Aeson..= (5000 :: Int) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` True

  describe "output truncation" $ do
    it "truncates very long output" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "command" Aeson..= ("yes | head -100000" :: Text) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` True
      T.length (result ^. trOutput) `shouldSatisfy` (< 60000)
      result ^. trOutput `shouldSatisfy` T.isInfixOf "truncated"

  describe "argument validation" $ do
    it "fails with missing command" $ do
      ctx <- newToolContext
      let args = Aeson.object []
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "Invalid arguments"

    it "fails with invalid argument type" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "command" Aeson..= (123 :: Int) ]
      result <- runExecutor bashTool ctx args
      result ^. trSuccess `shouldBe` False
