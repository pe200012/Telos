module Telos.SnapshotSpec ( spec ) where

import           Control.Exception    ( try )

import qualified Data.Text            as T

import           Control.Lens           ( (^.) )

import           Relude

import           System.Directory     ( getCurrentDirectory )
import           System.IO.Temp       ( withSystemTempDirectory )

import           Telos.Snapshot

import           Test.Hspec

spec :: Spec
spec = do
  describe "Snapshot" $ do
    describe "getProjectHash" $ do
      it "returns consistent hash for same path" $ do
        cwd <- getCurrentDirectory
        hash1 <- getProjectHash cwd
        hash2 <- getProjectHash cwd
        hash1 `shouldBe` hash2

      it "returns 16 character hash" $ do
        cwd <- getCurrentDirectory
        hash <- getProjectHash cwd
        T.length hash `shouldBe` 16

      it "returns different hash for different paths" $ do
        hash1 <- getProjectHash "/tmp/project1"
        hash2 <- getProjectHash "/tmp/project2"
        hash1 `shouldNotBe` hash2

    describe "initSnapshotConfig" $ do
      it "returns disabled config when enabled=False" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig False tmpDir
          (config ^. scEnabled) `shouldBe` False

      it "returns enabled config with valid gitDir when enabled=True" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          (config ^. scEnabled) `shouldBe` True
          (config ^. scGitDir) `shouldSatisfy` (not . null)

    describe "takeSnapshot" $ do
      it "returns Nothing when disabled" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          let config = SnapshotConfig False tmpDir ""
          result <- takeSnapshot config
          result `shouldBe` Nothing

      it "returns Just tree hash when enabled" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          -- Create a file to snapshot
          writeFile (tmpDir <> "/test.txt") "hello"
          result <- takeSnapshot config
          result `shouldSatisfy` isJust

      it "returns different hashes for different file contents" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          -- First snapshot
          writeFile (tmpDir <> "/test.txt") "hello"
          Just hash1 <- takeSnapshot config
          -- Second snapshot with different content
          writeFile (tmpDir <> "/test.txt") "world"
          Just hash2 <- takeSnapshot config
          hash1 `shouldNotBe` hash2

    describe "restoreFiles" $ do
      it "restores file to previous state" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          -- Create original file and snapshot
          writeFile (tmpDir <> "/test.txt") "original"
          Just hash1 <- takeSnapshot config
          -- Modify file
          writeFile (tmpDir <> "/test.txt") "modified"
          _ <- takeSnapshot config
          -- Verify modified
          content1 <- readFileBS (tmpDir <> "/test.txt")
          content1 `shouldBe` "modified"
          -- Restore to original
          result <- restoreFiles config hash1 ["test.txt"]
          result `shouldBe` Right ()
          -- Verify restored
          content2 <- readFileBS (tmpDir <> "/test.txt")
          content2 `shouldBe` "original"

      it "deletes file that was created after snapshot" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          -- Create initial file and snapshot
          writeFile (tmpDir <> "/existing.txt") "exists"
          Just hash1 <- takeSnapshot config
          -- Create new file after snapshot
          writeFile (tmpDir <> "/new.txt") "new content"
          _ <- takeSnapshot config
          -- Restore - new file should be deleted
          result <- restoreFiles config hash1 ["new.txt"]
          result `shouldBe` Right ()
          -- Verify file was deleted
          exists <- doesFileExist (tmpDir <> "/new.txt")
          exists `shouldBe` False

      it "returns error when disabled" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          let config = SnapshotConfig False tmpDir ""
          result <- restoreFiles config "somehash" ["test.txt"]
          result `shouldSatisfy` isLeft

    describe "fileExistsInTree" $ do
      it "returns True for file in tree" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          writeFile (tmpDir <> "/test.txt") "hello"
          Just hash <- takeSnapshot config
          exists <- fileExistsInTree (config ^. scGitDir) hash "test.txt"
          exists `shouldBe` True

      it "returns False for file not in tree" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          writeFile (tmpDir <> "/test.txt") "hello"
          Just hash <- takeSnapshot config
          exists <- fileExistsInTree (config ^. scGitDir) hash "nonexistent.txt"
          exists `shouldBe` False

    describe "getDiff" $ do
      it "returns empty diff for identical snapshots" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          writeFile (tmpDir <> "/test.txt") "hello"
          Just hash <- takeSnapshot config
          diff <- getDiff config hash hash
          diff `shouldBe` ""

      it "returns non-empty diff for different snapshots" $ do
        withSystemTempDirectory "telos-test" $ \tmpDir -> do
          config <- initSnapshotConfig True tmpDir
          writeFile (tmpDir <> "/test.txt") "hello"
          Just hash1 <- takeSnapshot config
          writeFile (tmpDir <> "/test.txt") "world"
          Just hash2 <- takeSnapshot config
          diff <- getDiff config hash1 hash2
          diff `shouldSatisfy` (not . T.null)

-- Helper to check file existence
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
  result <- try @SomeException $ readFileBS path
  pure $ isRight result
