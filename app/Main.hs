module Main ( main ) where

import           Config                   ( Config, loadConfig )

import           Control.Monad.IO.Class   ( liftIO )

import           Data.IORef               ( IORef, newIORef, readIORef, writeIORef )
import           Data.Maybe               ( fromMaybe )
import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )
import           Effects.Snapshot         ( Snapshot(..), loadSnapshot, saveSnapshot )

import           LLM.Http                 ( Message(..), Role(User, Assistant), runLLMHttp )

import           Polysemy                 ( Embed, Member, Sem, embed, interpret, runM )
import           Polysemy.Input           ( runInputConst )
import           Polysemy.State           ( modify, runState )

import           System.Console.Haskeline ( InputT
                                          , defaultSettings
                                          , getInputLine
                                          , outputStrLn
                                          , runInputT
                                          )

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> do
      snapshotRef <- newIORef Nothing
      runInputT defaultSettings (loop cfg snapshotRef)
  where
    loop :: Config -> IORef (Maybe [ Message ]) -> InputT IO ()
    loop cfg snapshotRef = do
      minput <- getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          response <- liftIO $ runM $ runInputConst cfg $ runSnapshotIO snapshotRef $ do
            initialHistory <- fromMaybe [] <$> loadSnapshot
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
          loop cfg snapshotRef

runSnapshotIO
  :: Member (Embed IO) r => IORef (Maybe [ Message ]) -> Sem (Snapshot ': r) a -> Sem r a
runSnapshotIO ref = interpret $ \case
  SaveSnapshot history -> embed $ writeIORef ref (Just history)
  LoadSnapshot         -> embed $ readIORef ref
