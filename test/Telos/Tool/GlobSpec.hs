module Telos.Tool.GlobSpec ( spec ) where

import           Test.Hspec

import           Control.Exception    ( IOException, bracket_, catch )
import qualified Data.Aeson           as Aeson
import qualified Data.Text            as T
import           Lens.Micro           ( (^.) )
import           System.Directory     ( createDirectoryIfMissing, removeDirectoryRecursive
                                      , getTemporaryDirectory
                                      )
import           System.FilePath      ( (</>) )

import           Telos.Tool.Glob      ( globTool )
import           Telos.Tool.Types     ( BuiltinTool(..), newToolContext, trSuccess, trOutput )

spec :: Spec
spec = describe "GlobTool" $ do
  describe "pattern matching" $ do
    it "finds files matching simple pattern" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test1.txt") "content1"
        writeFile (dir </> "test2.txt") "content2"
        writeFile (dir </> "other.md") "content3"
        ctx <- newToolContext
        let args = Aeson.object
              [ "pattern" Aeson..= ("*.txt" :: Text)
              , "path" Aeson..= toText dir
              ]
        result <- (_btExecutor globTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "test1.txt"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "test2.txt"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "other.md")

    it "finds files in subdirectories with **" $ do
      withTempDir $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub1")
        createDirectoryIfMissing True (dir </> "sub2")
        writeFile (dir </> "root.hs") ""
        writeFile (dir </> "sub1" </> "file1.hs") ""
        writeFile (dir </> "sub2" </> "file2.hs") ""
        ctx <- newToolContext
        let args = Aeson.object
              [ "pattern" Aeson..= ("**/*.hs" :: Text)
              , "path" Aeson..= toText dir
              ]
        result <- (_btExecutor globTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "file1.hs"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "file2.hs"

  describe "empty results" $ do
    it "returns empty for no matches" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test.txt") ""
        ctx <- newToolContext
        let args = Aeson.object
              [ "pattern" Aeson..= ("*.xyz" :: Text)
              , "path" Aeson..= toText dir
              ]
        result <- (_btExecutor globTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "No files"

  describe "invalid directory" $ do
    it "fails for non-existent directory" $ do
      ctx <- newToolContext
      let args = Aeson.object
            [ "pattern" Aeson..= ("*.txt" :: Text)
            , "path" Aeson..= ("/nonexistent_dir_12345" :: Text)
            ]
      result <- (_btExecutor globTool) ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "not found"

  describe "default path" $ do
    it "uses current directory when path not specified" $ do
      ctx <- newToolContext
      let args = Aeson.object ["pattern" Aeson..= ("*.cabal" :: Text)]
      result <- (_btExecutor globTool) ctx args
      result ^. trSuccess `shouldBe` True

  describe "argument validation" $ do
    it "fails with missing pattern" $ do
      ctx <- newToolContext
      let args = Aeson.object ["path" Aeson..= ("/tmp" :: Text)]
      result <- (_btExecutor globTool) ctx args
      result ^. trSuccess `shouldBe` False

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> "telos-test-glob"
  bracket_
    (createDirectoryIfMissing True dir)
    (safeRemoveDir dir)
    (action dir)

safeRemoveDir :: FilePath -> IO ()
safeRemoveDir path = removeDirectoryRecursive path `catch` ignoreErr
  where
    ignoreErr :: IOException -> IO ()
    ignoreErr _ = pure ()
