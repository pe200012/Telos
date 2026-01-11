module Telos.Tool.BashSpec ( spec ) where

import           Test.Hspec

import qualified Data.Aeson         as Aeson
import qualified Data.Text          as T
import           Lens.Micro         ( (^.) )

import           Telos.Tool.Bash    ( bashTool )
import           Telos.Tool.Types   ( BuiltinTool(..), newToolContext, trSuccess, trOutput )

spec :: Spec
spec = describe "BashTool" $ do
  describe "command execution" $ do
    it "executes simple echo command" $ do
      ctx <- newToolContext
      let args = Aeson.object ["command" Aeson..= ("echo hello" :: Text)]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "hello"

    it "returns exit code in success flag" $ do
      ctx <- newToolContext
      let args = Aeson.object ["command" Aeson..= ("exit 1" :: Text)]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` False

    it "captures stderr output" $ do
      ctx <- newToolContext
      let args = Aeson.object ["command" Aeson..= ("echo error >&2" :: Text)]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "error"

  describe "workdir parameter" $ do
    it "executes command in specified directory" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "command" Aeson..= ("pwd" :: Text)
            , "workdir" Aeson..= ("/tmp" :: Text)
            ]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` True
      result ^. trOutput `shouldSatisfy` T.isInfixOf "/tmp"

    it "fails for non-existent directory" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "command" Aeson..= ("pwd" :: Text)
            , "workdir" Aeson..= ("/nonexistent_dir_12345" :: Text)
            ]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` False

  describe "timeout parameter" $ do
    it "times out long-running command" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "command" Aeson..= ("sleep 10" :: Text)
            , "timeout" Aeson..= (100 :: Int)
            ]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "timed out"

    it "completes fast command within timeout" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "command" Aeson..= ("echo fast" :: Text)
            , "timeout" Aeson..= (5000 :: Int)
            ]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` True

  describe "output truncation" $ do
    it "truncates very long output" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "command" Aeson..= ("yes | head -100000" :: Text)
            ]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` True
      T.length (result ^. trOutput) `shouldSatisfy` (< 60000)
      result ^. trOutput `shouldSatisfy` T.isInfixOf "truncated"

  describe "argument validation" $ do
    it "fails with missing command" $ do
      ctx <- newToolContext
      let args = Aeson.object []
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "Invalid arguments"

    it "fails with invalid argument type" $ do
      ctx <- newToolContext
      let args = Aeson.object ["command" Aeson..= (123 :: Int)]
      result <- (_btExecutor bashTool) ctx args
      result ^. trSuccess `shouldBe` False
