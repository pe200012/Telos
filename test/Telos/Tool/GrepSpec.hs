module Telos.Tool.GrepSpec ( spec ) where

import           Control.Exception ( IOException, bracket_, catch )

import qualified Data.Aeson        as Aeson
import qualified Data.Text         as T

import           Lens.Micro        ( (^.) )

import           Relude

import           System.Directory  ( createDirectoryIfMissing
                                   , getTemporaryDirectory
                                   , removeDirectoryRecursive
                                   )
import           System.FilePath   ( (</>) )

import           Telos.Tool.Grep   ( grepTool )
import           Telos.Tool.Types  ( BuiltinTool(..), newToolContext, trOutput, trSuccess )

import           Test.Hspec

spec :: Spec
spec = describe "GrepTool" $ do
  describe "regex search" $ do
    it "finds simple pattern matches" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test.txt") "hello world\nfoo bar\nhello again"
        ctx <- newToolContext
        let args
              = Aeson.object [ "pattern" Aeson..= ("hello" :: Text), "path" Aeson..= toText dir ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "hello world"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "hello again"

    it "supports regex patterns" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test.txt") "foo123bar\nfoo456bar\nbaz789qux"
        ctx <- newToolContext
        let args
              = Aeson.object
                [ "pattern" Aeson..= ("foo[0-9]+bar" :: Text), "path" Aeson..= toText dir ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "foo123bar"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "foo456bar"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "baz789qux")

  describe "include filter" $ do
    it "filters files by extension" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "code.hs") "hello haskell"
        writeFile (dir </> "code.py") "hello python"
        ctx <- newToolContext
        let args
              = Aeson.object
                [ "pattern" Aeson..= ("hello" :: Text)
                , "path" Aeson..= toText dir
                , "include" Aeson..= ("*.hs" :: Text)
                ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "haskell"
        result ^. trOutput `shouldSatisfy` (not . T.isInfixOf "python")

  describe "no matches" $ do
    it "returns empty result for no matches" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test.txt") "hello world"
        ctx <- newToolContext
        let args
              = Aeson.object [ "pattern" Aeson..= ("xyz123" :: Text), "path" Aeson..= toText dir ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "No matches"

  describe "subdirectories" $ do
    it "searches recursively" $ do
      withTempDir $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub")
        writeFile (dir </> "root.txt") "findme root"
        writeFile (dir </> "sub" </> "nested.txt") "findme nested"
        ctx <- newToolContext
        let args
              = Aeson.object [ "pattern" Aeson..= ("findme" :: Text), "path" Aeson..= toText dir ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "root"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "nested"

  describe "line numbers" $ do
    it "includes line numbers in output" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "test.txt") "line1\nmatch here\nline3"
        ctx <- newToolContext
        let args
              = Aeson.object [ "pattern" Aeson..= ("match" :: Text), "path" Aeson..= toText dir ]
        result <- (_btExecutor grepTool) ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf ":2:"

  describe "argument validation" $ do
    it "fails with missing pattern" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "path" Aeson..= ("/tmp" :: Text) ]
      result <- (_btExecutor grepTool) ctx args
      result ^. trSuccess `shouldBe` False

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> "telos-test-grep"
  bracket_ (createDirectoryIfMissing True dir) (safeRemoveDir dir) (action dir)

safeRemoveDir :: FilePath -> IO ()
safeRemoveDir path = removeDirectoryRecursive path `catch` ignoreErr
  where
    ignoreErr :: IOException -> IO ()
    ignoreErr _ = pure ()
