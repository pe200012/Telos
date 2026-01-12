{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Isolated snapshot storage for undo/redo functionality.
--
-- This module uses a separate Git repository (not the project's .git) to store
-- file snapshots. This avoids polluting the user's repository with dangling objects.
--
-- Architecture:
--   - Snapshot repo: ~/.local/share/telos/snapshots/{project-hash}/
--   - Uses git write-tree (tree objects only, no commits)
--   - GIT_DIR points to snapshot repo, GIT_WORK_TREE points to project
module Telos.Snapshot
  ( -- * Types
    Snapshot(..)
  , snapHash
  , snapFiles
  , snapDiff
  , SnapshotConfig(..)
  , scEnabled
  , scProjectPath
  , scGitDir
  
    -- * Initialization
  , initSnapshotConfig
  , initSnapshotRepo
  
    -- * Snapshot Operations
  , takeSnapshot
  , restoreFiles
  , restoreSnapshot
  , getDiff
  
    -- * Utilities
  , getSnapshotDir
  , getProjectHash
  , fileExistsInTree
  ) where

import           Control.Exception       ( IOException, catch )

import qualified Crypto.Hash.SHA256      as SHA256

import           Data.Aeson              ( FromJSON, ToJSON )
import qualified Data.ByteString.Base16  as Base16
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.IO            as TIO

import           Control.Lens           ( makeLenses )

import           Relude

import           System.Directory        ( XdgDirectory(XdgData)
                                         , canonicalizePath
                                         , createDirectoryIfMissing
                                         , doesDirectoryExist
                                         , getXdgDirectory
                                         , removeFile
                                         )
import           System.Exit             ( ExitCode(..) )
import           System.FilePath         ( (</>) )
import qualified System.Posix.Env
import           System.Process          ( CreateProcess(..)
                                         , proc
                                         , readCreateProcessWithExitCode
                                         )

-- | Snapshot of file state at a point in conversation.
data Snapshot = Snapshot
  { _snapHash  :: Text        -- ^ Git tree hash at this point
  , _snapFiles :: [FilePath]  -- ^ Files that were modified after this snapshot
  , _snapDiff  :: Text        -- ^ Unified diff of changes (for display/redo)
  } deriving stock ( Eq, Show, Generic )

instance FromJSON Snapshot
instance ToJSON Snapshot

makeLenses ''Snapshot

-- | Configuration for the snapshot system.
data SnapshotConfig = SnapshotConfig
  { _scEnabled     :: Bool        -- ^ Whether snapshots are enabled
  , _scProjectPath :: FilePath    -- ^ Project working directory
  , _scGitDir      :: FilePath    -- ^ Isolated git directory for snapshots
  } deriving stock ( Eq, Show, Generic )

instance FromJSON SnapshotConfig
instance ToJSON SnapshotConfig

makeLenses ''SnapshotConfig

-- | Get the snapshot git directory for a project.
-- Returns: ~/.local/share/telos/snapshots/{project-hash}/
getSnapshotDir :: FilePath -> IO FilePath
getSnapshotDir projectPath = do
  dataDir <- getXdgDirectory XdgData "telos"
  projHash <- getProjectHash projectPath
  pure $ dataDir </> "snapshots" </> toString projHash

-- | Hash project path for unique identification.
-- Uses first 16 chars of SHA256 for brevity.
getProjectHash :: FilePath -> IO Text
getProjectHash projectPath = do
  canonical <- canonicalizePath projectPath
  let digest = SHA256.hash (TE.encodeUtf8 $ toText canonical)
      hexDigest = TE.decodeUtf8 $ Base16.encode digest
  pure $ T.take 16 hexDigest

-- | Initialize snapshot configuration for a project.
initSnapshotConfig :: Bool -> FilePath -> IO SnapshotConfig
initSnapshotConfig enabled projectPath = do
  if enabled
    then do
      result <- initSnapshotRepo projectPath
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ "Warning: Could not init snapshots: " <> err
          pure $ SnapshotConfig False projectPath ""
        Right gitDir ->
          pure $ SnapshotConfig True projectPath gitDir
    else
      pure $ SnapshotConfig False projectPath ""

-- | Initialize isolated snapshot git repository for a project.
initSnapshotRepo :: FilePath -> IO (Either Text FilePath)
initSnapshotRepo projectPath = do
  snapDir <- getSnapshotDir projectPath
  exists <- doesDirectoryExist snapDir

  if exists
    then pure $ Right snapDir
    else do
      createDirectoryIfMissing True snapDir
      -- Use runGit' (no GIT_WORK_TREE) for init --bare
      result <- runGit' snapDir ["init", "--bare", snapDir]
      case result of
        Left err -> pure $ Left err
        Right _  -> pure $ Right snapDir

-- | Take a snapshot using git write-tree (tree object only, no commit).
takeSnapshot :: SnapshotConfig -> IO (Maybe Text)
takeSnapshot config
  | not (_scEnabled config) = pure Nothing
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config

      -- Stage all files (respecting .gitignore)
      _ <- runGit gitDir workTree ["add", "-A"]

      -- Create tree object (no commit)
      result <- runGit gitDir workTree ["write-tree"]
      case result of
        Left _     -> pure Nothing
        Right hash -> pure $ Just $ T.strip hash

-- | Restore specific files from a snapshot tree.
-- Files not in the snapshot (newly created) will be deleted.
restoreFiles :: SnapshotConfig -> Text -> [FilePath] -> IO (Either Text ())
restoreFiles config treeHash files
  | null files = pure $ Right ()
  | not (_scEnabled config) = pure $ Left "Snapshots are disabled"
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config

      forM_ files $ \file -> do
        -- Check if file exists in tree
        exists <- fileExistsInTree gitDir treeHash file

        if exists
          then do
            -- Restore from tree
            _ <- runGit gitDir workTree ["checkout", toString treeHash, "--", file]
            pure ()
          else do
            -- File was created after snapshot, delete it
            let fullPath = workTree </> file
            removeFile fullPath `catch` \(_ :: IOException) -> pure ()

      pure $ Right ()

-- | Restore entire working tree to snapshot state.
restoreSnapshot :: SnapshotConfig -> Text -> IO (Either Text ())
restoreSnapshot config treeHash
  | not (_scEnabled config) = pure $ Left "Snapshots are disabled"
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config

      -- Read tree into index
      result1 <- runGit gitDir workTree ["read-tree", toString treeHash]
      case result1 of
        Left err -> pure $ Left err
        Right _ -> do
          -- Checkout all files from index
          result2 <- runGit gitDir workTree ["checkout-index", "-a", "-f"]
          pure $ second (const ()) result2

-- | Get unified diff between two tree hashes.
getDiff :: SnapshotConfig -> Text -> Text -> IO Text
getDiff config fromHash toHash
  | not (_scEnabled config) = pure ""
  | otherwise = do
      let gitDir = _scGitDir config
          workTree = _scProjectPath config
      result <- runGit gitDir workTree ["diff", toString fromHash, toString toHash]
      case result of
        Left _    -> pure ""
        Right out -> pure out

-- | Check if a file exists in a tree object.
fileExistsInTree :: FilePath -> Text -> FilePath -> IO Bool
fileExistsInTree gitDir treeHash file = do
  result <- runGit' gitDir ["ls-tree", toString treeHash, "--", file]
  case result of
    Left _       -> pure False
    Right output -> pure $ not $ T.null $ T.strip output

-- | Run git command with isolated GIT_DIR and GIT_WORK_TREE.
runGit :: FilePath -> FilePath -> [String] -> IO (Either Text Text)
runGit gitDir workTree args = do
  existingEnv <- getEnvironment
  let gitEnv = [ ("GIT_DIR", gitDir)
               , ("GIT_WORK_TREE", workTree)
               ] <> existingEnv
      cp = (proc "git" args) { env = Just gitEnv }
  (exitCode, out, err) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess   -> pure $ Right $ toText out
    ExitFailure _ -> pure $ Left $ toText err

-- | Run git command with only GIT_DIR (for bare repo operations).
runGit' :: FilePath -> [String] -> IO (Either Text Text)
runGit' gitDir args = do
  existingEnv <- getEnvironment
  let gitEnv = [("GIT_DIR", gitDir)] <> existingEnv
      cp = (proc "git" args) { env = Just gitEnv }
  (exitCode, out, err) <- readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess   -> pure $ Right $ toText out
    ExitFailure _ -> pure $ Left $ toText err

-- | Get the current environment variables.
getEnvironment :: IO [(String, String)]
getEnvironment = System.Posix.Env.getEnvironment

