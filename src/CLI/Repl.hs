{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module CLI.Repl ( runRepl ) where

import           CLI.Commands             ( Command(..), parseCommand, renderHelp )
import           CLI.Context              ( ContextSpec
                                          , addContextPath
                                          , buildContextMessage
                                          , clearContextSpec
                                          , defaultContextSpec
                                          , maxBytes
                                          , maxPerFile
                                          , paths
                                          , validatePathSpec
                                          )

import           Config                   ( Config )

import           Control.Lens             ( (?~), (^.), non )

import           Data.Char                ( isAlphaNum )
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text

import           Effects.FileSystem       ( FileSystem )
import           Effects.LLM              ( askLLM )
import           Effects.Snapshot         ( SnapshotCommit
                                          , loadSnapshot
                                          , mkSnapshotCommit
                                          , saveSnapshot
                                          )

import           FileSystem.Local         ( runFileSystemLocal )

import           LLM.Http                 ( Role(..), runLLMHttp, runLLMHttpSilent )

import           Polysemy                 ( Embed, Members, Sem, embed, runM )
import           Polysemy.Embed           ( runEmbedded )
import           Polysemy.Input           ( Input, runInputConst )
import           Polysemy.State           ( State, evalState, get, modify, put, runState )

import           Relude                   hiding ( State, evalState, get, modify, put, runState )

import           Snapshot.Git             ( ProjectEntry
                                          , createProject
                                          , entryLastSession
                                          , lastSessionHash
                                          , latestSnapshotHash
                                          , listProjects
                                          , listSnapshots
                                          , projectName
                                          , projectPath
                                          , renameProject
                                          , runSnapshotGit
                                          , setLastSessionHash
                                          )

import           System.Console.Haskeline ( InputT
                                          , autoAddHistory
                                          , defaultSettings
                                          , getInputLine
                                          , historyFile
                                          , runInputT
                                          )
import           System.Directory         ( XdgDirectory(XdgCache)
                                          , createDirectoryIfMissing
                                          , getCurrentDirectory
                                          , getXdgDirectory
                                          )
import           System.FilePath          ( (</>) )

import           Types.Chat               ( Message, content, mkMessage, role )

type InputIO = InputT IO

runRepl :: Config -> IO ()
runRepl cfg = do
  scopeRoot <- getCurrentDirectory
  currentProject <- initCurrentProject scopeRoot
  lastRef <- lastSessionHash scopeRoot (currentProject ^. projectName)
  historyPath <- historyFilePath
  let settings = defaultSettings { historyFile = Just historyPath, autoAddHistory = True }
  runM
    $ runEmbedded @InputIO @IO (runInputT settings)
    $ runFileSystemLocal scopeRoot
    $ evalState defaultContextSpec
    $ evalState currentProject
    $ evalState (mkSnapshotCommit (lastRef ^. non "HEAD"))
    $ loop cfg scopeRoot

historyFilePath :: IO FilePath
historyFilePath = do
  cacheRoot <- getXdgDirectory XdgCache "telos"
  createDirectoryIfMissing True cacheRoot
  pure (cacheRoot </> "repl-history")

loop :: Members
       '[ State SnapshotCommit
        , State ProjectEntry
        , State ContextSpec
        , FileSystem
        , Embed InputIO
        , Embed IO
        ]
       r
     => Config
     -> FilePath
     -> Sem r ()
loop cfg scopeRoot = embed @InputIO (getInputLine "% ") >>= \case
  Nothing -> pure ()
  Just "quit" -> pure ()
  Just (Text.dropWhile (== ' ') . Text.pack -> input) -> do
    if Text.isPrefixOf ">" input
      then do
        let message = Text.stripStart (Text.drop 1 input)
        unless (Text.null message) $ do
          current <- get @SnapshotCommit
          currentProject <- get @ProjectEntry
          contextSpec <- get @ContextSpec
          runInputConst cfg $ do
            contextMsg <- buildContextMessage scopeRoot contextSpec
            ( updatedHistory, _ ) <- runSnapshotGit scopeRoot (currentProject ^. projectName) $ do
              initial <- loadSnapshot current
              let startingHistory   = initial ^. non []
                  historyForRequest
                    = maybe startingHistory (\ctx -> startingHistory <> [ ctx ]) contextMsg
              ( updatedHistory, reply ) <- runState historyForRequest $ do
                let userMsg = mkMessage User message
                modify (userMsg :)
                reply <- runLLMHttp $ askLLM userMsg
                modify (mkMessage Assistant reply :)
                pure reply
              let savedHistory = stripContext updatedHistory
              saveSnapshot savedHistory
              pure ( savedHistory, reply )
            maybeGenerateTitle scopeRoot updatedHistory
            pure ()
      else do
        case parseCommand input of
          Left err  -> unless (Text.null err) $ embed @IO $ putStrLn (Text.unpack err)
          Right cmd -> do
            handled <- handleCommand scopeRoot cmd
            unless handled $ embed @IO $ putStrLn "Unknown command. Use > to chat."
    loop cfg scopeRoot

handleCommand
  :: Members '[ State SnapshotCommit, State ProjectEntry, State ContextSpec, Embed IO ] r
  => FilePath
  -> Command
  -> Sem r Bool
handleCommand scopeRoot command = case command of
  CmdSnapshot -> do
    project <- get @ProjectEntry
    mHash <- embed @IO $ latestSnapshotHash scopeRoot (project ^. projectName)
    case mHash of
      Nothing -> embed @IO $ putStrLn "No snapshots yet."
      Just h  -> embed @IO $ putStrLn (Text.unpack h)
    pure True
  CmdSnapshots -> do
    project <- get @ProjectEntry
    entries <- embed @IO $ listSnapshots scopeRoot (project ^. projectName)
    if null entries
      then embed @IO $ putStrLn "No snapshots yet."
      else embed @IO $ mapM_ (putStrLn . Text.unpack) entries
    pure True
  CmdHistory -> do
    project <- get @ProjectEntry
    current <- get @SnapshotCommit
    history <- runSnapshotGit scopeRoot (project ^. projectName) (loadSnapshot current)
    case history of
      Nothing       -> embed @IO $ putStrLn "No history yet."
      Just messages -> if null messages
        then embed @IO $ putStrLn "No history yet."
        else embed @IO $ mapM_ (putStrLn . renderMessage) (reverse messages)
    pure True
  CmdProjects -> do
    entries <- embed @IO $ listProjects scopeRoot
    if null entries
      then embed @IO $ putStrLn "No projects yet."
      else do
        let grouped
              = Map.fromListWith (<>) [ ( entry ^. projectPath, [ entry ] ) | entry <- entries ]
            ordered   = sortOn fst (Map.toList grouped)
            scopeText = Text.pack scopeRoot
        embed @IO $ for_ ordered $ \( path, items ) -> do
          putStrLn (Text.unpack path)
          let sorted = sortOn (^. projectName) items
          for_ sorted $ \entry -> putStrLn (renderProject scopeText path entry)
    pure True
  CmdContextShow -> do
    spec <- get @ContextSpec
    let limitLine
          = "limits: maxBytes="
          <> show (spec ^. maxBytes)
          <> " maxPerFile="
          <> show (spec ^. maxPerFile)
        pathLines
          = if null (spec ^. paths)
            then [ "(no paths)" ]
            else spec ^. paths
    embed @IO $ do
      putStrLn limitLine
      mapM_ putStrLn pathLines
    pure True
  CmdContextClear -> do
    modify @ContextSpec clearContextSpec
    embed @IO $ putStrLn "Context cleared."
    pure True
  CmdContextAdd raw -> do
    result <- embed @IO $ validatePathSpec scopeRoot raw
    case result of
      Left err   -> embed @IO $ putStrLn (Text.unpack err)
      Right path -> do
        modify @ContextSpec (addContextPath path)
        embed @IO $ putStrLn ("Context added: " <> path)
    pure True
  CmdProjectUse name -> do
    entries <- embed @IO $ listProjects scopeRoot
    let scopeText = Text.pack scopeRoot
    case find (\entry -> entry ^. projectName == name) entries of
      Nothing    -> embed @IO $ putStrLn "Unknown project name."
      Just entry -> if isRelated scopeText (entry ^. projectPath)
        then switchProject scopeRoot entry
        else embed @IO $ putStrLn "Project is not in the current folder."
    pure True
  CmdProjectNew mName mPath -> do
    let ( nameArg, pathArg ) = normalizeProjectNewArgs mName mPath
    createOrSwitch scopeRoot nameArg pathArg
    pure True
  CmdRestore hash -> do
    project <- get @ProjectEntry
    put @SnapshotCommit (mkSnapshotCommit hash)
    embed @IO $ setLastSessionHash scopeRoot (project ^. projectName) hash
    put @ProjectEntry (project & entryLastSession ?~ hash)
    embed @IO $ putStrLn ("Restored snapshot: " <> Text.unpack hash)
    pure True
  CmdHelp mCommand -> do
    embed @IO $ putStrLn (Text.unpack (renderHelp mCommand))
    pure True

renderProject :: Text -> Text -> ProjectEntry -> String
renderProject scopeText groupPath entry
  = let
      lastText = maybe "-" Text.unpack (entry ^. entryLastSession)
      nameText = Text.unpack (entry ^. projectName)
    in 
      if groupPath == scopeText
        then "  " <> nameText <> "  " <> lastText
        else "  " <> Text.unpack groupPath <> "  " <> nameText <> "  " <> lastText

isRelated :: Text -> Text -> Bool
isRelated scopeText pathText = pathText == scopeText || Text.isPrefixOf (scopeText <> "/") pathText

renderMessage :: Message -> String
renderMessage msg
  = let
      label = case msg ^. role of
        System    -> "System"
        User      -> "User"
        Assistant -> "Assistant"
    in 
      label <> ": " <> Text.unpack (msg ^. content)

createOrSwitch :: Members '[ State SnapshotCommit, State ProjectEntry, Embed IO ] r
               => FilePath
               -> Maybe Text
               -> Maybe FilePath
               -> Sem r ()
createOrSwitch rootPath mName mPath = do
  entries <- embed @IO $ listProjects rootPath
  let existingNames = map (^. projectName) entries
      name          = case mName of
        Just value -> value
        Nothing    -> nextPlaceholderName existingNames
      path          = mPath ^. non rootPath
  created <- embed @IO $ createProject rootPath name path
  case created of
    Left err    -> embed @IO $ putStrLn (Text.unpack err)
    Right entry -> switchProject rootPath entry

normalizeProjectNewArgs :: Maybe Text -> Maybe Text -> ( Maybe Text, Maybe FilePath )
normalizeProjectNewArgs firstArg secondArg = case ( firstArg, secondArg ) of
  ( Nothing, Nothing )     -> ( Nothing, Nothing )
  ( Just token, Nothing )  -> if looksLikePath token
    then ( Nothing, Just (Text.unpack token) )
    else ( Just token, Nothing )
  ( Just name, Just path ) -> ( Just name, Just (Text.unpack path) )
  ( Nothing, Just path )   -> ( Nothing, Just (Text.unpack path) )

looksLikePath :: Text -> Bool
looksLikePath token
  = Text.isPrefixOf "/" token
  || Text.isPrefixOf "./" token
  || Text.isPrefixOf "../" token
  || Text.isPrefixOf "~/" token

switchProject :: Members '[ State SnapshotCommit, State ProjectEntry, Embed IO ] r
              => FilePath
              -> ProjectEntry
              -> Sem r ()
switchProject rootPath entry = do
  lastRef <- embed @IO $ lastSessionHash rootPath (entry ^. projectName)
  put @ProjectEntry entry
  put @SnapshotCommit (mkSnapshotCommit (lastRef ^. non "HEAD"))
  embed @IO $ putStrLn ("Switched project: " <> Text.unpack (entry ^. projectName))

initCurrentProject :: FilePath -> IO ProjectEntry
initCurrentProject scopeRoot = do
  entries <- listProjects scopeRoot
  let cwdText  = Text.pack scopeRoot
      samePath = filter (\entry -> entry ^. projectPath == cwdText) entries
  case preferNamed samePath of
    Just entry -> pure entry
    Nothing    -> do
      let placeholder = nextPlaceholderName (map (^. projectName) entries)
      created <- createProject scopeRoot placeholder scopeRoot
      case created of
        Left err    -> error err
        Right entry -> pure entry

preferNamed :: [ ProjectEntry ] -> Maybe ProjectEntry
preferNamed entries = case filter (not . isPlaceholderName . (^. projectName)) entries of
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
  let name = current ^. projectName
  when (shouldGenerateTitle name history) $ do
    titleText <- generateTitle history
    case sanitizeTitle titleText of
      Nothing      -> pure ()
      Just desired -> do
        entries <- embed @IO $ listProjects scopeRoot
        let existing   = filter (/= name) (map (^. projectName) entries)
            uniqueName = makeUniqueName desired existing
        unless (uniqueName == name) $ do
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
userMessageCount history = length (filter (\msg -> msg ^. role == User) history)

titleThreshold :: Int
titleThreshold = 3

generateTitle :: Members '[ Embed IO, Input Config ] r => [ Message ] -> Sem r Text
generateTitle history = do
  let recent = reverse (take 6 history)
      prompt = titlePrompt recent
  ( _, title ) <- runState @[ Message ] [] $ runLLMHttpSilent $ askLLM (mkMessage User prompt)
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
      label = case msg ^. role of
        System    -> "System"
        User      -> "User"
        Assistant -> "Assistant"
    in 
      label <> ": " <> Text.take 200 (msg ^. content)

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

stripContext :: [ Message ] -> [ Message ]
stripContext = filter (not . isContextMessage)

isContextMessage :: Message -> Bool
isContextMessage msg = msg ^. role == System && Text.isPrefixOf "<filesystem>" (msg ^. content)
