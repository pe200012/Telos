{-# LANGUAGE MonoLocalBinds #-}

module Main ( main ) where

import           Config                   ( Config, loadConfig )

import           Control.Lens             ( (^.), non )
import           Control.Monad            ( when )

import           Data.Char                ( isAlphaNum )
import           Data.Foldable            ( for_ )
import           Data.List                ( find, sortOn )
import qualified Data.Map.Strict          as Map
import           Data.Maybe               ( listToMaybe )
import           Data.Text                ( Text )
import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )
import           Effects.Snapshot         ( SnapshotCommit(SnapshotCommit)
                                          , loadSnapshot
                                          , saveSnapshot
                                          )

import           LLM.Http                 ( Message(..), Role(..), runLLMHttp, runLLMHttpSilent )

import           Polysemy                 ( Embed, Members, Sem, embed, runM )
import           Polysemy.Embed           ( runEmbedded )
import           Polysemy.Input           ( Input, runInputConst )
import           Polysemy.State           ( State, evalState, get, modify, put, runState )

import           Snapshot.Git             ( ProjectEntry(..)
                                          , createProject
                                          , lastSessionHash
                                          , latestSnapshotHash
                                          , listProjects
                                          , listSnapshots
                                          , renameProject
                                          , runSnapshotGit
                                          , setLastSessionHash
                                          )

import           System.Console.Haskeline ( InputT, defaultSettings, getInputLine, runInputT )
import           System.Directory         ( getCurrentDirectory )

type InputIO = InputT IO

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> do
      scopeRoot <- getCurrentDirectory
      currentProject <- initCurrentProject scopeRoot
      lastRef <- lastSessionHash scopeRoot (_projectName currentProject)
      runM
        $ runEmbedded @InputIO @IO (runInputT defaultSettings)
        $ evalState currentProject
        $ evalState (SnapshotCommit (lastRef ^. non "HEAD"))
        $ loop cfg scopeRoot
  where
    loop :: Members '[ State SnapshotCommit, State ProjectEntry, Embed InputIO, Embed IO ] r
         => Config
         -> FilePath
         -> Sem r ()
    loop cfg scopeRoot = do
      minput <- embed @InputIO $ getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          handled <- handleCommand scopeRoot (Text.pack input)
          if handled
            then pure ()
            else do
              current <- get @SnapshotCommit
              currentProject <- get @ProjectEntry
              runInputConst cfg $ do
                ( updatedHistory, _ ) <- runSnapshotGit scopeRoot (_projectName currentProject) $ do
                  initial <- loadSnapshot current
                  let startingHistory = initial ^. non []
                  ( updatedHistory, reply ) <- runState startingHistory $ do
                    let userMsg = Message { _role = User, _content = Text.pack input }
                    modify (userMsg :)
                    reply <- runLLMHttp $ askLLM userMsg
                    modify (Message { _role = Assistant, _content = reply } :)
                    pure reply
                  saveSnapshot updatedHistory
                  pure ( updatedHistory, reply )
                maybeGenerateTitle scopeRoot updatedHistory
                pure ()
          loop cfg scopeRoot

handleCommand :: Members '[ State SnapshotCommit, State ProjectEntry, Embed IO ] r
              => FilePath
              -> Text
              -> Sem r Bool
handleCommand scopeRoot input
  | input == "/snapshot" = do
    project <- get @ProjectEntry
    mHash <- embed @IO $ latestSnapshotHash scopeRoot (_projectName project)
    case mHash of
      Nothing -> embed @IO $ putStrLn "No snapshots yet."
      Just h  -> embed @IO $ putStrLn (Text.unpack h)
    pure True
  | input == "/snapshots" = do
    project <- get @ProjectEntry
    entries <- embed @IO $ listSnapshots scopeRoot (_projectName project)
    if null entries
      then embed @IO $ putStrLn "No snapshots yet."
      else embed @IO $ mapM_ (putStrLn . Text.unpack) entries
    pure True
  | input == "/history" = do
    project <- get @ProjectEntry
    current <- get @SnapshotCommit
    history <- runSnapshotGit scopeRoot (_projectName project) (loadSnapshot current)
    case history of
      Nothing       -> embed @IO $ putStrLn "No history yet."
      Just messages -> if null messages
        then embed @IO $ putStrLn "No history yet."
        else embed @IO $ mapM_ (putStrLn . renderMessage) (reverse messages)
    pure True
  | input == "/projects" = do
    entries <- embed @IO $ listProjects scopeRoot
    if null entries
      then embed @IO $ putStrLn "No projects yet."
      else do
        let grouped
              = Map.fromListWith (<>) [ ( _projectPath entry, [ entry ] ) | entry <- entries ]
            ordered   = sortOn fst (Map.toList grouped)
            scopeText = Text.pack scopeRoot
        embed @IO $ for_ ordered $ \( path, items ) -> do
          putStrLn (Text.unpack path)
          let sorted = sortOn _projectName items
          for_ sorted $ \entry -> putStrLn (renderProject scopeText path entry)
    pure True
  | Just rest <- Text.stripPrefix "/project use " input = do
    let name = Text.strip rest
    if Text.null name
      then embed @IO $ putStrLn "Usage: /project use <name>"
      else do
        entries <- embed @IO $ listProjects scopeRoot
        let scopeText = Text.pack scopeRoot
        case find (\entry -> _projectName entry == name) entries of
          Nothing    -> embed @IO $ putStrLn "Unknown project name."
          Just entry -> if isRelated scopeText (_projectPath entry)
            then switchProject scopeRoot entry
            else embed @IO $ putStrLn "Project is not in the current folder."
    pure True
  | input == "/project new" = do
    createOrSwitch scopeRoot Nothing Nothing
    pure True
  | Just rest <- Text.stripPrefix "/project new " input = do
    let args = Text.strip rest
    if Text.null args
      then embed @IO $ putStrLn "Usage: /project new [name] [path]"
      else do
        let ( mName, mPath ) = parseNewArgs args
        createOrSwitch scopeRoot mName mPath
    pure True
  | Just rest <- Text.stripPrefix "/restore " input = do
    let hash = Text.strip rest
    if Text.null hash
      then embed @IO $ putStrLn "Usage: /restore <hash>"
      else do
        project <- get @ProjectEntry
        put @SnapshotCommit (SnapshotCommit hash)
        embed @IO $ setLastSessionHash scopeRoot (_projectName project) hash
        put @ProjectEntry project { _entryLastSession = Just hash }
        embed @IO $ putStrLn ("Restored snapshot: " <> Text.unpack hash)
    pure True
  | otherwise = pure False
  where
    renderProject scopeText groupPath entry
      = let
          lastText = maybe "-" Text.unpack (_entryLastSession entry)
          nameText = Text.unpack (_projectName entry)
        in 
          if groupPath == scopeText
            then "  " <> nameText <> "  " <> lastText
            else "  " <> Text.unpack groupPath <> "  " <> nameText <> "  " <> lastText

    isRelated scopeText pathText
      = pathText == scopeText || Text.isPrefixOf (scopeText <> "/") pathText

    renderMessage msg
      = let
          label = case _role msg of
            System    -> "System"
            User      -> "User"
            Assistant -> "Assistant"
        in 
          label <> ": " <> Text.unpack (_content msg)

    createOrSwitch rootPath mName mPath = do
      entries <- embed @IO $ listProjects rootPath
      let existingNames = map _projectName entries
          name          = case mName of
            Just value -> value
            Nothing    -> nextPlaceholderName existingNames
          path          = mPath ^. non rootPath
      created <- embed @IO $ createProject rootPath name path
      case created of
        Left err    -> embed @IO $ putStrLn (Text.unpack err)
        Right entry -> switchProject rootPath entry

    parseNewArgs args = case Text.words args of
      []          -> ( Nothing, Nothing )
      [ token ]   -> if looksLikePath token
        then ( Nothing, Just (Text.unpack token) )
        else ( Just token, Nothing )
      name : rest -> ( Just name, Just (Text.unpack (Text.unwords rest)) )

    looksLikePath token
      = Text.isPrefixOf "/" token
      || Text.isPrefixOf "./" token
      || Text.isPrefixOf "../" token
      || Text.isPrefixOf "~/" token

    switchProject rootPath entry = do
      lastRef <- embed @IO $ lastSessionHash rootPath (_projectName entry)
      put @ProjectEntry entry
      put @SnapshotCommit (SnapshotCommit (lastRef ^. non "HEAD"))
      embed @IO $ putStrLn ("Switched project: " <> Text.unpack (_projectName entry))

initCurrentProject :: FilePath -> IO ProjectEntry
initCurrentProject scopeRoot = do
  entries <- listProjects scopeRoot
  let cwdText  = Text.pack scopeRoot
      samePath = filter (\entry -> _projectPath entry == cwdText) entries
  case preferNamed samePath of
    Just entry -> pure entry
    Nothing    -> do
      let placeholder = nextPlaceholderName (map _projectName entries)
      created <- createProject scopeRoot placeholder scopeRoot
      case created of
        Left err    -> error (Text.unpack err)
        Right entry -> pure entry

preferNamed :: [ ProjectEntry ] -> Maybe ProjectEntry
preferNamed entries = case filter (not . isPlaceholderName . _projectName) entries of
  (entry : _) -> Just entry
  []          -> listToMaybe entries

nextPlaceholderName :: [ Text ] -> Text
nextPlaceholderName existing = go (1 :: Int)
  where
    go n
      = let
          candidate = "untitled-" <> Text.pack (show n)
        in 
          if candidate `elem` existing
            then go (n + 1)
            else candidate

maybeGenerateTitle :: Members '[ State ProjectEntry, Embed IO, Input Config ] r
                   => FilePath
                   -> [ Message ]
                   -> Sem r ()
maybeGenerateTitle scopeRoot history = do
  current <- get @ProjectEntry
  let name = _projectName current
  when (shouldGenerateTitle name history) $ do
    titleText <- generateTitle history
    case sanitizeTitle titleText of
      Nothing      -> pure ()
      Just desired -> do
        entries <- embed @IO $ listProjects scopeRoot
        let existing   = filter (/= name) (map _projectName entries)
            uniqueName = makeUniqueName desired existing
        if uniqueName == name
          then pure ()
          else do
            renamed <- embed @IO $ renameProject scopeRoot name uniqueName
            case renamed of
              Left err    -> embed @IO $ putStrLn ("Project rename failed: " <> Text.unpack err)
              Right entry -> do
                put @ProjectEntry entry
                embed @IO $ putStrLn ("Project titled: " <> Text.unpack uniqueName)

shouldGenerateTitle :: Text -> [ Message ] -> Bool
shouldGenerateTitle name history
  = isPlaceholderName name && userMessageCount history >= titleThreshold

isPlaceholderName :: Text -> Bool
isPlaceholderName = Text.isPrefixOf "untitled-"

userMessageCount :: [ Message ] -> Int
userMessageCount history = length (filter (\msg -> _role msg == User) history)

titleThreshold :: Int
titleThreshold = 3

generateTitle :: Members '[ Embed IO, Input Config ] r => [ Message ] -> Sem r Text
generateTitle history = do
  let recent = reverse (take 6 history)
      prompt = titlePrompt recent
  ( _, title ) <- runState @[ Message ] []
    $ runLLMHttpSilent
    $ askLLM Message { _role = User, _content = prompt }
  pure title

titlePrompt :: [ Message ] -> Text
titlePrompt history
  = let
      header
        = [ "Generate a brief project title for this chat."
          , "Return 2-4 words, ASCII only."
          , "Use letters, numbers, spaces, or hyphens."
          , "Return only the title on a single line."
          , "Conversation:"
          ]
    in 
      Text.unlines (header <> map formatMessage history)

formatMessage :: Message -> Text
formatMessage msg
  = let
      label = case _role msg of
        System    -> "System"
        User      -> "User"
        Assistant -> "Assistant"
    in 
      label <> ": " <> Text.take 200 (_content msg)

sanitizeTitle :: Text -> Maybe Text
sanitizeTitle raw
  = let
      line       = Text.takeWhile (/= '\n') (Text.strip raw)
      cleaned    = Text.filter (\c -> isAlphaNum c || c == ' ' || c == '-') line
      normalized = Text.unwords (Text.words cleaned)
      trimmed    = Text.take 60 normalized
    in 
      if Text.null trimmed
        then Nothing
        else Just trimmed

makeUniqueName :: Text -> [ Text ] -> Text
makeUniqueName desired existing
  = if desired `elem` existing
    then go (2 :: Int)
    else desired
  where
    go n
      = let
          candidate = desired <> "-" <> Text.pack (show n)
        in 
          if candidate `elem` existing
            then go (n + 1)
            else candidate
