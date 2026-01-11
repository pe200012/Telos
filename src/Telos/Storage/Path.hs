module Telos.Storage.Path
  ( getDataDir
  , getSessionsDir
  , getSessionDir
  , getSessionInfoPath
  , getMessagesPath
  , ensureSessionDir
  ) where

import           System.Directory  ( XdgDirectory(..)
                                   , createDirectoryIfMissing
                                   , getXdgDirectory
                                   )
import           System.FilePath   ( (</>) )

import           Telos.Storage.Types ( SessionId(..) )

getDataDir :: IO FilePath
getDataDir = getXdgDirectory XdgData "telos"

getSessionsDir :: IO FilePath
getSessionsDir = (</> "sessions") <$> getDataDir

getSessionDir :: SessionId -> IO FilePath
getSessionDir (SessionId sid) = (</> toString sid) <$> getSessionsDir

getSessionInfoPath :: SessionId -> IO FilePath
getSessionInfoPath sid = (</> "info.json") <$> getSessionDir sid

getMessagesPath :: SessionId -> IO FilePath
getMessagesPath sid = (</> "messages.jsonl") <$> getSessionDir sid

ensureSessionDir :: SessionId -> IO FilePath
ensureSessionDir sid = do
  sessionDir <- getSessionDir sid
  createDirectoryIfMissing True sessionDir
  pure sessionDir
