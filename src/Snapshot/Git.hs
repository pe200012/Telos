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
  , createProject
  , renameProject
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

data ProjectMeta = ProjectMeta { _uuid :: Text, _path :: Text, _lastSession :: Maybe Text }
  deriving ( Eq, Show, Generic )

newtype ProjectIndex = ProjectIndex { _projects :: Map Text ProjectMeta }
  deriving ( Eq, Show, Generic )

data ProjectEntry
  = ProjectEntry { _projectName :: Text, _projectPath :: Text, _entryLastSession :: Maybe Text }
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

runSnapshotGit :: Member (Embed IO) r => FilePath -> Text -> Sem (Snapshot ': r) a -> Sem r a
runSnapshotGit scopeRoot projectName = interpret $ \case
  SaveSnapshot history -> embed $ saveSnapshotGit scopeRoot projectName history
  LoadSnapshot (SnapshotCommit c) -> embed $ loadSnapshotGit scopeRoot projectName c

snapshotRepoPath :: FilePath -> Text -> IO FilePath
snapshotRepoPath scopeRoot projectName = do
  cacheRoot <- xdgCacheDir
  meta <- getProjectMeta scopeRoot projectName
  pure (cacheRoot </> "telos" </> "snapshots" </> Text.unpack (_uuid meta))

saveSnapshotGit :: FilePath -> Text -> [ Message ] -> IO ()
saveSnapshotGit scopeRoot projectName history = do
  repo <- snapshotRepoPath scopeRoot projectName
  ensureGitRepo repo
  let historyPath = repo </> "history.json"
  LBS.writeFile historyPath (encode history)
  _ <- git repo [ "add", "history.json" ]
  _ <- gitMaybe
    repo
    [ "-c", "user.name=telos", "-c", "user.email=telos@local", "commit", "-m", "snapshot" ]
  updateLastSession scopeRoot projectName

loadSnapshotGit :: FilePath -> Text -> Text -> IO (Maybe [ Message ])
loadSnapshotGit scopeRoot projectName commitHash = do
  repo <- snapshotRepoPath scopeRoot projectName
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

indexPath :: FilePath -> IO FilePath
indexPath scopeRoot = do
  cacheRoot <- xdgCacheDir
  let scopeHash = sha256Hex (Text.pack scopeRoot)
  pure (cacheRoot </> "telos" </> "project-index" </> Text.unpack scopeHash <> ".json")

loadIndex :: FilePath -> IO ProjectIndex
loadIndex scopeRoot = do
  path <- indexPath scopeRoot
  exists <- doesFileExist path
  if exists
    then readIndexFile path
    else pure (ProjectIndex Map.empty)

readIndexFile :: FilePath -> IO ProjectIndex
readIndexFile path = do
  bytes <- LBS.readFile path `catch` \e -> if isDoesNotExistError e
    then pure "{}"
    else throwIO e
  case eitherDecodeStrict' (LBS.toStrict bytes) of
    Left err  -> throwIO (userError ("Invalid project-index.json at " <> path <> ": " <> err))
    Right idx -> pure idx

saveIndex :: FilePath -> ProjectIndex -> IO ()
saveIndex scopeRoot idx = do
  path <- indexPath scopeRoot
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (encode idx)

createProject :: FilePath -> Text -> FilePath -> IO (Either Text ProjectEntry)
createProject scopeRoot name path = do
  idx <- loadIndex scopeRoot
  if Map.member name (_projects idx)
    then pure (Left "Project name already exists.")
    else do
      newId <- newProjectId
      let meta = ProjectMeta { _uuid = newId, _path = Text.pack path, _lastSession = Nothing }
          idx' = idx { _projects = Map.insert name meta (_projects idx) }
      saveIndex scopeRoot idx'
      pure (Right (ProjectEntry name (_path meta) Nothing))

renameProject :: FilePath -> Text -> Text -> IO (Either Text ProjectEntry)
renameProject scopeRoot oldName newName = do
  idx <- loadIndex scopeRoot
  if Map.member newName (_projects idx)
    then pure (Left "Project name already exists.")
    else case Map.lookup oldName (_projects idx) of
      Nothing   -> pure (Left "Project name not found.")
      Just meta -> do
        let idx' = idx { _projects = Map.insert newName meta (Map.delete oldName (_projects idx)) }
        saveIndex scopeRoot idx'
        pure (Right (ProjectEntry newName (_path meta) (_lastSession meta)))

listProjects :: FilePath -> IO [ ProjectEntry ]
listProjects scopeRoot = map toEntry . Map.toList . _projects <$> loadIndex scopeRoot
  where
    toEntry ( name, meta )
      = ProjectEntry
      { _projectName = name, _projectPath = _path meta, _entryLastSession = _lastSession meta }

getProjectMeta :: FilePath -> Text -> IO ProjectMeta
getProjectMeta scopeRoot projectName = do
  idx <- loadIndex scopeRoot
  case Map.lookup projectName (_projects idx) of
    Nothing   -> throwIO (userError ("Unknown project name: " <> Text.unpack projectName))
    Just meta -> pure meta

updateLastSession :: FilePath -> Text -> IO ()
updateLastSession scopeRoot projectName = do
  repo <- snapshotRepoPath scopeRoot projectName
  result <- gitMaybe repo [ "rev-parse", "HEAD" ]
  case result of
    Left _    -> pure ()
    Right out -> do
      let commitHash = Text.pack (takeWhile (/= '\n') out)
      setLastSessionHash scopeRoot projectName commitHash

lastSessionHash :: FilePath -> Text -> IO (Maybe Text)
lastSessionHash scopeRoot projectName = do
  idx <- loadIndex scopeRoot
  pure $ Map.lookup projectName (_projects idx) >>= _lastSession

setLastSessionHash :: FilePath -> Text -> Text -> IO ()
setLastSessionHash scopeRoot projectName commitHash = do
  idx <- loadIndex scopeRoot
  case Map.lookup projectName (_projects idx) of
    Nothing   -> throwIO (userError ("Unknown project name: " <> Text.unpack projectName))
    Just meta -> do
      let meta' = meta { _lastSession = Just commitHash }
          idx'  = idx { _projects = Map.insert projectName meta' (_projects idx) }
      saveIndex scopeRoot idx'

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

sha256Hex :: Text -> Text
sha256Hex payload
  = let
      digest   = hash (encodeUtf8 payload) :: Digest SHA256
      hexBytes = convertToBase Base16 digest
    in 
      Text.pack (BS8.unpack hexBytes)

newProjectId :: IO Text
newProjectId = sha256Hex . Text.pack . show <$> getCurrentTime

latestSnapshotHash :: FilePath -> Text -> IO (Maybe Text)
latestSnapshotHash scopeRoot projectName = do
  repo <- snapshotRepoPath scopeRoot projectName
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure Nothing
    else do
      result <- gitMaybe repo [ "rev-parse", "HEAD" ]
      case result of
        Left _    -> pure Nothing
        Right out -> pure (Just (Text.pack (takeWhile (/= '\n') out)))

listSnapshots :: FilePath -> Text -> IO [ Text ]
listSnapshots scopeRoot projectName = do
  repo <- snapshotRepoPath scopeRoot projectName
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure []
    else do
      result <- gitMaybe repo [ "--no-pager", "log", "--pretty=format:%H %s" ]
      case result of
        Left _    -> pure []
        Right out -> pure (Text.lines (Text.pack out))
