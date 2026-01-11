module Telos.Tool.WriteSpec ( spec ) where

import           Test.Hspec

import           Control.Exception    ( IOException, bracket_, catch )
import qualified Data.Aeson           as Aeson
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lens.Micro           ( (^.) )
import           System.Directory     ( removeFile, removeDirectoryRecursive
                                      , doesFileExist, getTemporaryDirectory
                                      )
import           System.FilePath      ( (</>) )

import           Telos.Tool.Read      ( readTool )
import           Telos.Tool.Write     ( writeTool )
import           Telos.Tool.Types     ( BuiltinTool(..), newToolContext, trSuccess, trOutput, markFileRead )

spec :: Spec
spec = describe "WriteTool" $ do
  describe "new file creation" $ do
    it "creates new file with content" $ do
      tmpDir <- getTemporaryDirectory
      let path = tmpDir </> "telos-test-write-new.txt"
      bracket_ (pure ()) (safeRemove path) $ do
        ctx <- newToolContext
        let args = Aeson.object
              [ "path" Aeson..= toText path
              , "content" Aeson..= ("hello world" :: Text)
              ]
        result <- (_btExecutor writeTool) ctx args
        result ^. trSuccess `shouldBe` True
        content <- TIO.readFile path
        content `shouldBe` "hello world"

    it "creates parent directories if needed" $ do
      tmpDir <- getTemporaryDirectory
      let dir = tmpDir </> "telos-test-nested"
          path = dir </> "subdir" </> "file.txt"
      bracket_ (pure ()) (safeRemoveDir dir) $ do
        ctx <- newToolContext
        let args = Aeson.object
              [ "path" Aeson..= toText path
              , "content" Aeson..= ("nested content" :: Text)
              ]
        result <- (_btExecutor writeTool) ctx args
        result ^. trSuccess `shouldBe` True
        exists <- doesFileExist path
        exists `shouldBe` True

  describe "overwrite existing file" $ do
    it "requires file to be read first" $ do
      withExistingFile "original" $ \path -> do
        ctx <- newToolContext
        let args = Aeson.object
              [ "path" Aeson..= path
              , "content" Aeson..= ("new content" :: Text)
              ]
        result <- (_btExecutor writeTool) ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "must Read"

    it "allows overwrite after reading" $ do
      withExistingFile "original" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args = Aeson.object
              [ "path" Aeson..= path
              , "content" Aeson..= ("new content" :: Text)
              ]
        result <- (_btExecutor writeTool) ctx args
        result ^. trSuccess `shouldBe` True
        content <- TIO.readFile (toString path)
        content `shouldBe` "new content"

    it "allows overwrite after using ReadTool" $ do
      withExistingFile "original" $ \path -> do
        ctx <- newToolContext
        let readArgs = Aeson.object ["path" Aeson..= path]
        _ <- (_btExecutor readTool) ctx readArgs
        let writeArgs = Aeson.object
              [ "path" Aeson..= path
              , "content" Aeson..= ("updated" :: Text)
              ]
        result <- (_btExecutor writeTool) ctx writeArgs
        result ^. trSuccess `shouldBe` True

  describe "argument validation" $ do
    it "fails with missing path" $ do
      ctx <- newToolContext
      let args = Aeson.object ["content" Aeson..= ("hello" :: Text)]
      result <- (_btExecutor writeTool) ctx args
      result ^. trSuccess `shouldBe` False

    it "fails with missing content" $ do
      ctx <- newToolContext
      let args = Aeson.object ["path" Aeson..= ("/tmp/test.txt" :: Text)]
      result <- (_btExecutor writeTool) ctx args
      result ^. trSuccess `shouldBe` False

withExistingFile :: Text -> (Text -> IO a) -> IO a
withExistingFile content action = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "telos-test-existing.txt"
  bracket_
    (TIO.writeFile path content)
    (safeRemove path)
    (action (toText path))

safeRemove :: FilePath -> IO ()
safeRemove path = do
  exists <- doesFileExist path
  when exists $ removeFile path

safeRemoveDir :: FilePath -> IO ()
safeRemoveDir path = removeDirectoryRecursive path `catch` ignoreNotFound
  where
    ignoreNotFound :: IOException -> IO ()
    ignoreNotFound _ = pure ()
