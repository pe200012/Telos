{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Snapshot.Git
  ( ProjectEntry(..)
  , runSnapshotGit
  , snapshotRepoPath
  , latestSnapshotHash
  , listProjects
  , listSnapshots
  , lastSessionHash
  , ensureProject
  , setLastSessionHash
  ) where

import           Control.Exception       ( catch, throwIO )
import           Control.Lens.TH         ( makeFieldsNoPrefix )
import           Control.Monad           ( unless )

import           Crypto.Hash             ( Digest, SHA256, hash )

import           Data.Aeson              ( FromJSON(parseJSON)
                                         , ToJSON(toJSON)
                                         , defaultOptions
                                         , eitherDecodeStrict'
                                         , encode
                                         , fieldLabelModifier
                                         , genericParseJSON
                                         , genericToJSON
                                         )
import           Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Map.Strict         ( Map )
import qualified Data.Map.Strict         as Map
import           Data.Text               ( Text )
import qualified Data.Text               as Text
import           Data.Text.Encoding      ( encodeUtf8 )
import           Data.Time.Clock         ( getCurrentTime )

import           Effects.Snapshot        ( Snapshot(..), SnapshotCommit(SnapshotCommit) )

import           GHC.Generics            ( Generic )

import           Polysemy                ( Embed, Member, Sem, embed, interpret )

import           System.Directory        ( createDirectoryIfMissing
                                         , doesDirectoryExist
                                         , doesFileExist
                                         , getHomeDirectory
                                         )
import           System.Environment      ( lookupEnv )
import           System.FilePath         ( (</>), takeDirectory )
import           System.IO.Error         ( isDoesNotExistError )
import           System.Process          ( readProcess )

import           Types.Chat              ( Message )

data ProjectMeta = ProjectMeta { _uuid :: Text, _lastSession :: Maybe Text }
  deriving ( Eq, Show, Generic )

newtype ProjectIndex = ProjectIndex { _projects :: Map Text ProjectMeta }
  deriving ( Eq, Show, Generic )

data ProjectEntry
  = ProjectEntry { _projectRoot :: Text, _projectId :: Text, _entryLastSession :: Maybe Text }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ProjectMeta

makeFieldsNoPrefix ''ProjectIndex

makeFieldsNoPrefix ''ProjectEntry

instance ToJSON ProjectMeta where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ProjectMeta where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ProjectIndex where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ProjectIndex where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

runSnapshotGit :: Member (Embed IO) r => FilePath -> Sem (Snapshot ': r) a -> Sem r a
runSnapshotGit rootPath = interpret $ \case
  SaveSnapshot history -> embed $ saveSnapshotGit rootPath history
  LoadSnapshot (SnapshotCommit c) -> embed $ loadSnapshotGit rootPath c

snapshotRepoPath :: FilePath -> IO FilePath
snapshotRepoPath rootPath = do
  cacheRoot <- xdgCacheDir
  projId <- getOrCreateProjectId rootPath
  pure (cacheRoot </> "telos" </> "snapshots" </> Text.unpack projId)

saveSnapshotGit :: FilePath -> [ Message ] -> IO ()
saveSnapshotGit rootPath history = do
  repo <- snapshotRepoPath rootPath
  ensureGitRepo repo
  let historyPath = repo </> "history.json"
  LBS.writeFile historyPath (encode history)
  _ <- git repo [ "add", "history.json" ]
  _ <- gitMaybe
    repo
    [ "-c", "user.name=telos", "-c", "user.email=telos@local", "commit", "-m", "snapshot" ]
  updateLastSession rootPath

loadSnapshotGit :: FilePath -> Text -> IO (Maybe [ Message ])
loadSnapshotGit rootPath commitHash = do
  repo <- snapshotRepoPath rootPath
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure Nothing
    else do
      let ref = Text.unpack commitHash <> ":history.json"
      contentResult <- gitMaybe repo [ "show", ref ]
      case contentResult of
        Left _        -> pure Nothing
        Right content -> case eitherDecodeStrict' (encodeUtf8 (Text.pack content)) of
          Left _        -> pure Nothing
          Right history -> pure (Just history)

ensureGitRepo :: FilePath -> IO ()
ensureGitRepo repo = do
  createDirectoryIfMissing True repo
  exists <- doesDirectoryExist (repo </> ".git")
  unless exists $ do
    _ <- git repo [ "init" ]
    pure ()

indexPath :: IO FilePath
indexPath = do
  cacheRoot <- xdgCacheDir
  pure (cacheRoot </> "telos" </> "project-index.json")

loadIndex :: IO ProjectIndex
loadIndex = do
  path <- indexPath
  exists <- doesFileExist path
  if not exists
    then pure (ProjectIndex Map.empty)
    else do
      bytes <- LBS.readFile path `catch` \e -> if isDoesNotExistError e
        then pure "{}"
        else throwIO e
      case eitherDecodeStrict' (LBS.toStrict bytes) of
        Left err  -> throwIO (userError ("Invalid project-index.json at " <> path <> ": " <> err))
        Right idx -> pure idx

saveIndex :: ProjectIndex -> IO ()
saveIndex idx = do
  path <- indexPath
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encode idx)

getOrCreateProjectId :: FilePath -> IO Text
getOrCreateProjectId rootPath = do
  idx <- loadIndex
  let key = Text.pack rootPath
  case Map.lookup key (_projects idx) of
    Just meta -> pure (_uuid meta)
    Nothing   -> do
      newId <- newProjectId
      let meta = ProjectMeta { _uuid = newId, _lastSession = Nothing }
          idx' = idx { _projects = Map.insert key meta (_projects idx) }
      saveIndex idx'
      pure newId

ensureProject :: FilePath -> IO Text
ensureProject = getOrCreateProjectId

listProjects :: IO [ ProjectEntry ]
listProjects = map toEntry . Map.toList . _projects <$> loadIndex
  where
    toEntry ( root, meta )
      = ProjectEntry
      { _projectRoot = root, _projectId = _uuid meta, _entryLastSession = _lastSession meta }

updateLastSession :: FilePath -> IO ()
updateLastSession rootPath = do
  repo <- snapshotRepoPath rootPath
  result <- gitMaybe repo [ "rev-parse", "HEAD" ]
  case result of
    Left _    -> pure ()
    Right out -> do
      let commitHash = Text.pack (takeWhile (/= '\n') out)
      setLastSessionHash rootPath commitHash

lastSessionHash :: FilePath -> IO (Maybe Text)
lastSessionHash rootPath = do
  idx <- loadIndex
  let key = Text.pack rootPath
  pure $ Map.lookup key (_projects idx) >>= _lastSession

setLastSessionHash :: FilePath -> Text -> IO ()
setLastSessionHash rootPath commitHash = do
  idx <- loadIndex
  let key = Text.pack rootPath
  projId <- getOrCreateProjectId rootPath
  let meta = case Map.lookup key (_projects idx) of
        Just m  -> m { _lastSession = Just commitHash }
        Nothing -> ProjectMeta { _uuid = projId, _lastSession = Just commitHash }
      idx' = idx { _projects = Map.insert key meta (_projects idx) }
  saveIndex idx'

xdgCacheDir :: IO FilePath
xdgCacheDir = do
  mCache <- lookupEnv "XDG_CACHE_HOME"
  case mCache of
    Just path -> pure path
    Nothing   -> do
      home <- getHomeDirectory
      pure (home </> ".cache")

git :: FilePath -> [ String ] -> IO String
git cwd args = readProcess "git" ("-C" : cwd : args) ""

gitMaybe :: FilePath -> [ String ] -> IO (Either IOError String)
gitMaybe cwd args = (Right <$> git cwd args) `catch` \e -> if isDoesNotExistError e
  then throwIO e
  else pure (Left e)

newProjectId :: IO Text
newProjectId = do
  now <- getCurrentTime
  let payload  = Text.pack (show now)
      digest   = hash (encodeUtf8 payload) :: Digest SHA256
      hexBytes = convertToBase Base16 digest
  pure (Text.pack (BS8.unpack hexBytes))

latestSnapshotHash :: FilePath -> IO (Maybe Text)
latestSnapshotHash rootPath = do
  repo <- snapshotRepoPath rootPath
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure Nothing
    else do
      result <- gitMaybe repo [ "rev-parse", "HEAD" ]
      case result of
        Left _    -> pure Nothing
        Right out -> pure (Just (Text.pack (takeWhile (/= '\n') out)))

listSnapshots :: FilePath -> IO [ Text ]
listSnapshots rootPath = do
  repo <- snapshotRepoPath rootPath
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure []
    else do
      result <- gitMaybe repo [ "--no-pager", "log", "--pretty=format:%H %s" ]
      case result of
        Left _    -> pure []
        Right out -> pure (Text.lines (Text.pack out))
