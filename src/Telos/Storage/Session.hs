module Telos.Storage.Session
  ( createSession
  , getSession
  , listSessions
  , deleteSession
  , touchSession
  , appendMessage
  , loadMessages
  , getMessageCount
  , saveContextMessages
  , loadContextMessages
  ) where

import           Control.Exception          ( IOException, catch, evaluate )

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import           Data.Time                  ( getCurrentTime )

import           Control.Lens                 ( (^.), non )

import           Relude

import           System.Directory           ( doesDirectoryExist
                                            , listDirectory
                                            , removeDirectoryRecursive
                                            )

import           Telos.Core.Types           ( Message )
import           Telos.Storage.Path
import           Telos.Storage.Types

createSession :: Maybe Text -> IO SessionInfo
createSession mTitle = do
  sid <- generateSessionId
  let title = mTitle ^. non "Untitled Session"
  info <- makeSessionInfo sid title

  _ <- ensureSessionDir sid
  infoPath <- getSessionInfoPath sid
  BL.writeFile infoPath (Aeson.encode info)

  msgsPath <- getMessagesPath sid
  writeFile msgsPath ""

  pure info

getSession :: SessionId -> IO (Maybe SessionInfo)
getSession sid = do
  infoPath <- getSessionInfoPath sid
  catch (readSessionInfo infoPath) handleNotFound
  where
    readSessionInfo path = do
      content <- readFileStrict path
      case Aeson.eitherDecode content of
        Left err   -> error $ "Failed to parse session info: " <> toText err
        Right info -> pure (Just info)

    handleNotFound :: IOException -> IO (Maybe SessionInfo)
    handleNotFound _ = pure Nothing

readFileStrict :: FilePath -> IO BL.ByteString
readFileStrict path = do
  content <- BL.readFile path
  _ <- evaluate (BL.length content)
  pure content

listSessions :: IO [ SessionInfo ]
listSessions = do
  sessionsDir <- getSessionsDir
  exists <- doesDirectoryExist sessionsDir
  if not exists
    then pure []
    else do
      entries <- listDirectory sessionsDir
      sessions <- forM entries $ \entry -> getSession (SessionId (T.pack entry))
      pure $ sortBy (comparing (Down . _siUpdatedAt)) (catMaybes sessions)

deleteSession :: SessionId -> IO ()
deleteSession sid = do
  sessionDir <- getSessionDir sid
  exists <- doesDirectoryExist sessionDir
  when exists $ removeDirectoryRecursive sessionDir

touchSession :: SessionId -> IO ()
touchSession sid = do
  mInfo <- getSession sid
  case mInfo of
    Nothing   -> pure ()
    Just info -> do
      now <- getCurrentTime
      let updated = info { _siUpdatedAt = now }
      infoPath <- getSessionInfoPath sid
      BL.writeFile infoPath (Aeson.encode updated)

appendMessage :: SessionId -> Message -> IO ()
appendMessage sid msg = do
  now <- getCurrentTime
  msgsPath <- getMessagesPath sid
  count <- getMessageCount sid
  let stored = StoredMessage count msg now
      line   = BLC.unpack (Aeson.encode stored) <> "\n"

  appendFile msgsPath line
  touchSession sid

loadMessages :: SessionId -> IO [ Message ]
loadMessages sid = do
  msgsPath <- getMessagesPath sid
  catch (parseMessages msgsPath) handleNotFound
  where
    parseMessages path = do
      content <- readFileStrict path
      let lns = BLC.lines content
      pure $ mapMaybe parseLine lns

    parseLine line
      | BL.null line = Nothing
      | otherwise = case Aeson.eitherDecode line of
        Left _       -> Nothing
        Right stored -> Just (_smMessage stored)

    handleNotFound :: IOException -> IO [ Message ]
    handleNotFound _ = pure []

getMessageCount :: SessionId -> IO Int
getMessageCount sid = length <$> loadMessages sid

saveContextMessages :: SessionId -> [ Message ] -> IO ()
saveContextMessages sid history = do
  existingCount <- getMessageCount sid
  let newMessages = drop existingCount history
  forM_ newMessages $ appendMessage sid

loadContextMessages :: SessionId -> IO [ Message ]
loadContextMessages = loadMessages
