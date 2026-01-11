module Telos.Tool.ReadSpec ( spec ) where

import           Control.Exception ( bracket_ )

import qualified Data.Aeson        as Aeson
import qualified Data.ByteString   as BS
import qualified Data.Text         as T

import           Lens.Micro        ( (^.) )

import           Relude

import           System.Directory  ( getTemporaryDirectory, removeFile )
import           System.FilePath   ( (</>) )

import           Telos.Tool.Read   ( readTool )
import           Telos.Tool.Types  ( BuiltinTool(..), newToolContext, trOutput, trSuccess )

import           Test.Hspec

spec :: Spec
spec = describe "ReadTool" $ do
  describe "normal file reading" $ do
    it "reads text file with line numbers" $ do
      withTempFile "line1\nline2\nline3\n" $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "1\tline1"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "2\tline2"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "3\tline3"

    it "reads empty file" $ do
      withTempFile "" $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True

  describe "offset and limit" $ do
    it "respects offset parameter" $ do
      withTempFile "line1\nline2\nline3\nline4\n" $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path, "offset" Aeson..= (2 :: Int) ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "3\tline3"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "1\tline1")

    it "respects limit parameter" $ do
      withTempFile "line1\nline2\nline3\nline4\n" $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path, "limit" Aeson..= (2 :: Int) ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "1\tline1"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "2\tline2"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "3\tline3")

    it "combines offset and limit" $ do
      withTempFile "a\nb\nc\nd\ne\n" $ \path -> do
        ctx <- newToolContext
        let args
              = Aeson.object
                [ "path" Aeson..= path, "offset" Aeson..= (1 :: Int), "limit" Aeson..= (2 :: Int) ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "2\tb"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "3\tc"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "1\ta")
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "4\td")

  describe "binary file detection" $ do
    it "rejects binary files with NUL bytes" $ do
      withTempBinaryFile (BS.pack [ 0x48, 0x65, 0x6c, 0x00, 0x6c, 0x6f ]) $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "binary"

  describe "file not found" $ do
    it "returns error for non-existent file" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "path" Aeson..= ("/nonexistent_file_12345.txt" :: Text) ]
      result <- (_btExecutor readTool) ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "not found"

  describe "line truncation" $ do
    it "truncates very long lines" $ do
      withTempFile (T.replicate 3000 "x") $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object [ "path" Aeson..= path ]
        result <- (_btExecutor readTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "..."
        T.length (result ^. trOutput) `shouldSatisfy` (< 2500)

  describe "argument validation" $ do
    it "fails with missing path" $ do
      ctx <- newToolContext
      let args = Aeson.object []
      result <- (_btExecutor readTool) ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "Invalid arguments"

withTempFile :: Text -> (Text -> IO a) -> IO a
withTempFile content action = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "telos-test-read.txt"
  bracket_ (writeFile path (toString content)) (removeFile path) (action (toText path))

withTempBinaryFile :: BS.ByteString -> (Text -> IO a) -> IO a
withTempBinaryFile content action = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "telos-test-binary.bin"
  bracket_ (BS.writeFile path content) (removeFile path) (action (toText path))
