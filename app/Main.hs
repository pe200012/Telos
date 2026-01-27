{-# LANGUAGE MonoLocalBinds #-}

module Main ( main ) where

import           Config                   ( Config, loadConfig )

import           Control.Lens             ( (^.), non )

import           Data.Foldable            ( for_ )
import           Data.Text                ( Text )
import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )
import           Effects.Snapshot         ( SnapshotCommit(SnapshotCommit)
                                          , loadSnapshot
                                          , saveSnapshot
                                          )

import           LLM.Http                 ( Message(..), Role(..), runLLMHttp )

import           Polysemy                 ( Embed, Members, Sem, embed, runM )
import           Polysemy.Embed           ( runEmbedded )
import           Polysemy.Input           ( runInputConst )
import           Polysemy.State           ( State, evalState, get, modify, put, runState )

import           Snapshot.Git             ( ProjectEntry(..)
                                          , ensureProject
                                          , lastSessionHash
                                          , latestSnapshotHash
                                          , listProjects
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

import           Text.Read                ( readMaybe )

type InputIO = InputT IO

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> do
      projectRoot <- getCurrentDirectory
      lastRef <- lastSessionHash projectRoot
      runM
        $ runEmbedded @InputIO @IO (runInputT defaultSettings)
        $ evalState projectRoot
        $ evalState (SnapshotCommit (lastRef ^. non "HEAD"))
        $ loop cfg
  where
    loop :: Members '[ State SnapshotCommit, State FilePath, Embed InputIO, Embed IO ] r
         => Config
         -> Sem r ()
    loop cfg = do
      minput <- embed @InputIO $ getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          handled <- handleCommand (Text.pack input)
          if handled
            then pure ()
            else do
              current <- get @SnapshotCommit
              projectRoot <- get @FilePath
              response <- runInputConst cfg $ runSnapshotGit projectRoot $ do
                initial <- loadSnapshot current
                let startingHistory = initial ^. non []
                ( updatedHistory, reply ) <- runState startingHistory $ do
                  let userMsg = Message { _role = User, _content = Text.pack input }
                  modify (userMsg :)
                  reply <- runLLMHttp $ askLLM userMsg
                  modify (Message { _role = Assistant, _content = reply } :)
                  pure reply
                saveSnapshot updatedHistory
                pure reply
              if Text.null response
                then pure ()
                else embed @InputIO $ outputStrLn (Text.unpack response)
          loop cfg

handleCommand
  :: Members '[ State SnapshotCommit, State FilePath, Embed IO ] r => Text -> Sem r Bool
handleCommand input
  | input == "/snapshot" = do
    projectRoot <- get @FilePath
    mHash <- embed @IO $ latestSnapshotHash projectRoot
    case mHash of
      Nothing -> embed @IO $ putStrLn "No snapshots yet."
      Just h  -> embed @IO $ putStrLn (Text.unpack h)
    pure True
  | input == "/snapshots" = do
    projectRoot <- get @FilePath
    entries <- embed @IO $ listSnapshots projectRoot
    if null entries
      then embed @IO $ putStrLn "No snapshots yet."
      else embed @IO $ mapM_ (putStrLn . Text.unpack) entries
    pure True
  | input == "/history" = do
    projectRoot <- get @FilePath
    current <- get @SnapshotCommit
    history <- runSnapshotGit projectRoot (loadSnapshot current)
    case history of
      Nothing       -> embed @IO $ putStrLn "No history yet."
      Just messages -> if null messages
        then embed @IO $ putStrLn "No history yet."
        else embed @IO $ mapM_ (putStrLn . renderMessage) (reverse messages)
    pure True
  | input == "/projects" = do
    cwd <- embed @IO getCurrentDirectory
    entries <- embed @IO listProjects
    let related = filter (isRelated cwd) entries
    if null related
      then embed @IO $ putStrLn "No related projects."
      else embed @IO $ mapM_ (putStrLn . renderProject) (zip [ 1 :: Int .. ] related)
    pure True
  | Just rest <- Text.stripPrefix "/project use " input = do
    let token = Text.strip rest
    if Text.null token
      then embed @IO $ putStrLn "Usage: /project use <index|path>"
      else do
        resolved <- resolveProject token
        for_ resolved switchProject
    pure True
  | input == "/project new" = do
    cwd <- embed @IO getCurrentDirectory
    _ <- embed @IO $ ensureProject cwd
    switchProject cwd
    pure True
  | Just rest <- Text.stripPrefix "/project new " input = do
    let token = Text.strip rest
    if Text.null token
      then embed @IO $ putStrLn "Usage: /project new [path]"
      else do
        let root = Text.unpack token
        _ <- embed @IO $ ensureProject root
        switchProject root
    pure True
  | Just rest <- Text.stripPrefix "/restore " input = do
    let hash = Text.strip rest
    if Text.null hash
      then embed @IO $ putStrLn "Usage: /restore <hash>"
      else do
        projectRoot <- get @FilePath
        put @SnapshotCommit (SnapshotCommit hash)
        embed @IO $ setLastSessionHash projectRoot hash
        embed @IO $ putStrLn ("Restored snapshot: " <> Text.unpack hash)
    pure True
  | otherwise = pure False
  where
    isRelated cwd entry
      = let
          cwdText = Text.pack cwd
          root    = _projectRoot entry
        in 
          root == cwdText || Text.isPrefixOf (cwdText <> "/") root

    renderProject ( idx, entry )
      = let
          lastText = maybe "-" Text.unpack (_entryLastSession entry)
        in 
          show idx <> ") " <> Text.unpack (_projectRoot entry) <> "  lastSession=" <> lastText

    renderMessage msg
      = let
          label = case _role msg of
            System    -> "System"
            User      -> "User"
            Assistant -> "Assistant"
        in 
          label <> ": " <> Text.unpack (_content msg)

    resolveProject token = case readMaybe (Text.unpack token) of
      Just idx -> do
        cwd <- embed @IO getCurrentDirectory
        entries <- embed @IO listProjects
        let related = filter (isRelated cwd) entries
        if idx < 1 || idx > length related
          then do
            embed @IO $ putStrLn "Invalid project index."
            pure Nothing
          else pure (Just (Text.unpack (_projectRoot (related !! (idx - 1)))))
      Nothing  -> pure (Just (Text.unpack token))

    switchProject root = do
      put @FilePath root
      lastRef <- embed @IO $ lastSessionHash root
      put @SnapshotCommit (SnapshotCommit (lastRef ^. non "HEAD"))
      embed @IO $ putStrLn ("Switched project: " <> root)
