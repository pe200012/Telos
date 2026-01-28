{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Snapshot.Git
  ( ProjectEntry
  , HasProjectName(..)
  , HasProjectPath(..)
  , HasEntryLastSession(..)
  , HasUuid(..)
  , HasPath(..)
  , HasLastSession(..)
  , HasProjects(..)
  , runSnapshotGit
  , snapshotRepoPath
  , latestSnapshotHash
  , listProjects
  , listSnapshots
  , lastSessionHash
  , createProject
  , renameProject
  , setLastSessionHash
  , newProjectMeta
  , newProjectIndex
  , newProjectEntry
  , defaultProjectIndex
  ) where

import           Control.Exception       ( IOException, catch, throwIO )
import           Control.Lens            ( (%~), (?~), (^.) )
import           Control.Lens.TH         ( makeFieldsNoPrefix )

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
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import           Data.Time.Clock         ( getCurrentTime )

import           Effects.Snapshot        ( HasUnSnapshotCommit(..), Snapshot(..) )

import           Polysemy                ( Embed, Member, Sem, embed, interpret )

import           Relude

import           System.Directory        ( createDirectoryIfMissing
                                         , doesDirectoryExist
                                         , doesFileExist
                                         , getHomeDirectory
                                         )
import           System.FilePath         ( (</>), takeDirectory )
import           System.IO.Error         ( isDoesNotExistError, userError )
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

newProjectMeta :: Text -> Text -> Maybe Text -> ProjectMeta
newProjectMeta = ProjectMeta

newProjectIndex :: Map Text ProjectMeta -> ProjectIndex
newProjectIndex = ProjectIndex

defaultProjectIndex :: ProjectIndex
defaultProjectIndex = ProjectIndex Map.empty

newProjectEntry :: Text -> Text -> Maybe Text -> ProjectEntry
newProjectEntry = ProjectEntry

instance ToJSON ProjectMeta where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ProjectMeta where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ProjectIndex where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ProjectIndex where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

runSnapshotGit :: Member (Embed IO) r => FilePath -> Text -> Sem (Snapshot ': r) a -> Sem r a
runSnapshotGit scopeRoot projName = interpret $ \case
  SaveSnapshot history -> embed $ saveSnapshotGit scopeRoot projName history
  LoadSnapshot commit  -> embed $ loadSnapshotGit scopeRoot projName (commit ^. unSnapshotCommit)

snapshotRepoPath :: FilePath -> Text -> IO FilePath
snapshotRepoPath scopeRoot projName = do
  cacheRoot <- xdgCacheDir
  meta <- getProjectMeta scopeRoot projName
  pure (cacheRoot </> "telos" </> "snapshots" </> Text.unpack (meta ^. uuid))

saveSnapshotGit :: FilePath -> Text -> [ Message ] -> IO ()
saveSnapshotGit scopeRoot projName history = do
  repo <- snapshotRepoPath scopeRoot projName
  ensureGitRepo repo
  let historyPath = repo </> "history.json"
  LBS.writeFile historyPath (encode history)
  _ <- git repo [ "add", "history.json" ]
  _ <- gitMaybe
    repo
    [ "-c", "user.name=telos", "-c", "user.email=telos@local", "commit", "-m", "snapshot" ]
  updateLastSession scopeRoot projName

loadSnapshotGit :: FilePath -> Text -> Text -> IO (Maybe [ Message ])
loadSnapshotGit scopeRoot projName commitHash = do
  repo <- snapshotRepoPath scopeRoot projName
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
  path' <- indexPath scopeRoot
  exists <- doesFileExist path'
  if exists
    then readIndexFile path'
    else pure defaultProjectIndex

readIndexFile :: FilePath -> IO ProjectIndex
readIndexFile path' = do
  bytes <- LBS.readFile path' `catch` \e -> if isDoesNotExistError e
    then pure "{}"
    else throwIO e
  case eitherDecodeStrict' (LBS.toStrict bytes) of
    Left err  -> throwIO (userError ("Invalid project-index.json at " <> path' <> ": " <> err))
    Right idx -> pure idx

saveIndex :: FilePath -> ProjectIndex -> IO ()
saveIndex scopeRoot idx = do
  path' <- indexPath scopeRoot
  createDirectoryIfMissing True (takeDirectory path')
  LBS.writeFile path' (encode idx)

createProject :: FilePath -> Text -> FilePath -> IO (Either Text ProjectEntry)
createProject scopeRoot name p = do
  idx <- loadIndex scopeRoot
  if Map.member name (idx ^. projects)
    then pure (Left "Project name already exists.")
    else do
      newId <- newProjectId
      let meta = newProjectMeta newId (Text.pack p) Nothing
          idx' = idx & projects %~ Map.insert name meta
      saveIndex scopeRoot idx'
      pure (Right (newProjectEntry name (meta ^. path) Nothing))

renameProject :: FilePath -> Text -> Text -> IO (Either Text ProjectEntry)
renameProject scopeRoot oldName newName = do
  idx <- loadIndex scopeRoot
  if Map.member newName (idx ^. projects)
    then pure (Left "Project name already exists.")
    else case Map.lookup oldName (idx ^. projects) of
      Nothing   -> pure (Left "Project name not found.")
      Just meta -> do
        let idx' = idx & projects %~ (Map.insert newName meta . Map.delete oldName)
        saveIndex scopeRoot idx'
        pure (Right (newProjectEntry newName (meta ^. path) (meta ^. lastSession)))

listProjects :: FilePath -> IO [ ProjectEntry ]
listProjects scopeRoot = map toEntry . Map.toList . (^. projects) <$> loadIndex scopeRoot
  where
    toEntry ( name, meta ) = newProjectEntry name (meta ^. path) (meta ^. lastSession)

getProjectMeta :: FilePath -> Text -> IO ProjectMeta
getProjectMeta scopeRoot projName = do
  idx <- loadIndex scopeRoot
  case Map.lookup projName (idx ^. projects) of
    Nothing   -> throwIO (userError ("Unknown project name: " <> Text.unpack projName))
    Just meta -> pure meta

updateLastSession :: FilePath -> Text -> IO ()
updateLastSession scopeRoot projName = do
  repo <- snapshotRepoPath scopeRoot projName
  result <- gitMaybe repo [ "rev-parse", "HEAD" ]
  case result of
    Left _    -> pure ()
    Right out -> do
      let commitHash = Text.pack (takeWhile (/= '\n') out)
      setLastSessionHash scopeRoot projName commitHash

lastSessionHash :: FilePath -> Text -> IO (Maybe Text)
lastSessionHash scopeRoot projName = do
  idx <- loadIndex scopeRoot
  let currentProjects = idx ^. projects
  pure $ Map.lookup projName currentProjects >>= (^. lastSession)

setLastSessionHash :: FilePath -> Text -> Text -> IO ()
setLastSessionHash scopeRoot projName commitHash = do
  idx <- loadIndex scopeRoot
  let currentProjects = idx ^. projects
  case Map.lookup projName currentProjects of
    Nothing   -> throwIO (userError ("Unknown project name: " <> Text.unpack projName))
    Just meta -> do
      let meta' = meta & lastSession ?~ commitHash
          idx'  = idx & projects %~ Map.insert projName meta'
      saveIndex scopeRoot idx'

xdgCacheDir :: IO FilePath
xdgCacheDir = do
  mCache <- lookupEnv "XDG_CACHE_HOME"
  case mCache of
    Just path'' -> pure path''
    Nothing     -> do
      home <- getHomeDirectory
      pure (home </> ".cache")

git :: FilePath -> [ String ] -> IO String
git cwd args = readProcess "git" ("-C" : cwd : args) ""

gitMaybe :: FilePath -> [ String ] -> IO (Either IOException String)
gitMaybe cwd args = (Right <$> git cwd args) `catch` \e -> if isDoesNotExistError e
  then throwIO e
  else pure (Left e)

sha256Hex :: Text -> Text
sha256Hex payload
  = let
      digest   = hash @ByteString (encodeUtf8 payload) :: Digest SHA256
      hexBytes = convertToBase Base16 digest
    in 
      Text.pack (BS8.unpack hexBytes)

newProjectId :: IO Text
newProjectId = sha256Hex . Text.pack . show <$> getCurrentTime

latestSnapshotHash :: FilePath -> Text -> IO (Maybe Text)
latestSnapshotHash scopeRoot projName = do
  repo <- snapshotRepoPath scopeRoot projName
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure Nothing
    else do
      result <- gitMaybe repo [ "rev-parse", "HEAD" ]
      case result of
        Left _    -> pure Nothing
        Right out -> pure (Just (Text.pack (takeWhile (/= '\n') out)))

listSnapshots :: FilePath -> Text -> IO [ Text ]
listSnapshots scopeRoot projName = do
  repo <- snapshotRepoPath scopeRoot projName
  exists <- doesDirectoryExist (repo </> ".git")
  if not exists
    then pure []
    else do
      result <- gitMaybe repo [ "--no-pager", "log", "--pretty=format:%H %s" ]
      case result of
        Left _    -> pure []
        Right out -> pure (Text.lines (Text.pack out))
