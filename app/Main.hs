{-# LANGUAGE MonoLocalBinds #-}

module Main ( main ) where

import           Config                   ( Config, loadConfig )

import           Control.Lens             ( (^.), non )
import           Control.Monad.IO.Class   ( liftIO )

import           Data.IORef               ( IORef, newIORef, readIORef, writeIORef )
import           Data.Maybe               ( fromMaybe )
import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )
import           Effects.Snapshot         ( loadSnapshot, saveSnapshot )

import           LLM.Http                 ( Message(..), Role(User, Assistant), runLLMHttp )

import           Polysemy                 ( runM )
import           Polysemy.Input           ( runInputConst )
import           Polysemy.State           ( modify, runState )

import           Snapshot.Git             ( lastSessionHash
                                          , latestSnapshotHash
                                          , listSnapshots
                                          , runSnapshotGit
                                          , setLastSessionHash
                                          )

import           System.Console.Haskeline ( InputT
                                          , defaultSettings
                                          , getInputLine
                                          , outputStrLn
                                          , runInputT
                                          )
import           System.Directory         ( getCurrentDirectory )

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> do
      projectRoot <- getCurrentDirectory
      lastRef <- lastSessionHash projectRoot
      snapshotRef <- newIORef (lastRef ^. non "HEAD")
      runInputT defaultSettings (loop cfg projectRoot snapshotRef)
  where
    loop :: Config -> FilePath -> IORef Text.Text -> InputT IO ()
    loop cfg projectRoot snapshotRef = do
      minput <- getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          handled <- liftIO $ handleCommand projectRoot snapshotRef (Text.pack input)
          if handled
            then pure ()
            else do
              currentRef <- liftIO $ readIORef snapshotRef
              response <- liftIO $ runM $ runInputConst cfg $ runSnapshotGit projectRoot $ do
                initialHistory <- fromMaybe [] <$> loadSnapshot currentRef
                ( updatedHistory, reply ) <- runState initialHistory $ do
                  let userMsg = Message { _role = User, _content = Text.pack input }
                  modify (userMsg :)
                  reply <- runLLMHttp $ askLLM userMsg
                  modify (Message { _role = Assistant, _content = reply } :)
                  pure reply
                saveSnapshot updatedHistory
                pure reply
              if Text.null response
                then pure ()
                else outputStrLn (Text.unpack response)
          loop cfg projectRoot snapshotRef

handleCommand :: FilePath -> IORef Text.Text -> Text.Text -> IO Bool
handleCommand projectRoot snapshotRef input
  | input == "/snapshot" = do
    mHash <- latestSnapshotHash projectRoot
    case mHash of
      Nothing -> putStrLn "No snapshots yet."
      Just h  -> putStrLn (Text.unpack h)
    pure True
  | input == "/snapshots" = do
    entries <- listSnapshots projectRoot
    if null entries
      then putStrLn "No snapshots yet."
      else mapM_ (putStrLn . Text.unpack) entries
    pure True
  | Just rest <- Text.stripPrefix "/restore " input = do
    let hash = Text.strip rest
    if Text.null hash
      then putStrLn "Usage: /restore <hash>"
      else do
        writeIORef snapshotRef hash
        setLastSessionHash projectRoot hash
        putStrLn ("Restored snapshot: " <> Text.unpack hash)
    pure True
  | otherwise = pure False
