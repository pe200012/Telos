{-# LANGUAGE GADTs #-}

module CLI.Commands ( Command(..), parseCommand, renderHelp ) where

import qualified Data.Text           as Text

import           Options.Applicative

import           Relude

data Command
  = CmdSnapshot
  | CmdSnapshots
  | CmdHistory
  | CmdProjects
  | CmdProjectUse Text
  | CmdProjectNew (Maybe Text) (Maybe Text)
  | CmdRestore Text
  | CmdContextAdd Text
  | CmdContextShow
  | CmdContextClear
  | CmdHelp (Maybe Text)
  deriving ( Eq, Show )

parseCommand :: Text -> Either Text Command
parseCommand input
  | Text.null (Text.strip input) = Left ""
  | otherwise = case execParserPure defaultPrefs commandInfo args of
    Success cmd         -> Right cmd
    Failure failure     -> Left (Text.pack (fst (renderFailure failure replName)))
    CompletionInvoked _ -> Left "Completion is not supported."
  where
    args = map Text.unpack (Text.words input)

renderHelp :: Maybe Text -> Text
renderHelp mCommand = case execParserPure defaultPrefs commandInfo args of
  Failure failure     -> Text.pack (fst (renderFailure failure replName))
  CompletionInvoked _ -> "Completion is not supported."
  Success _           -> "No help available."
  where
    args = case mCommand of
      Nothing   -> [ "--help" ]
      Just name -> [ Text.unpack name, "--help" ]

commandInfo :: ParserInfo Command
commandInfo
  = info
    (commandParser <**> helper)
    (fullDesc
     <> progDesc "REPL commands"
     <> header "Telos REPL"
     <> footer "Use > to chat. CLI flags are not available here.")

commandParser :: Parser Command
commandParser
  = hsubparser
    (command "snapshot" (info (pure CmdSnapshot) (progDesc "Show latest snapshot hash"))
     <> command "snapshots" (info (pure CmdSnapshots) (progDesc "List snapshot commits"))
     <> command "history" (info (pure CmdHistory) (progDesc "Show chat history"))
     <> command "projects" (info (pure CmdProjects) (progDesc "List projects"))
     <> command "project" (info projectParser (progDesc "Project commands"))
     <> command "restore" (info restoreParser (progDesc "Restore snapshot by hash"))
     <> command "context" (info contextParser (progDesc "Context commands"))
     <> command "help" (info helpParser (progDesc "Show help")))

projectParser :: Parser Command
projectParser
  = hsubparser
    (command "use" (info projectUseParser (progDesc "Switch project by name"))
     <> command "new" (info projectNewParser (progDesc "Create a new project")))

projectUseParser :: Parser Command
projectUseParser = CmdProjectUse <$> argument textArgument (metavar "NAME")

projectNewParser :: Parser Command
projectNewParser
  = CmdProjectNew <$> optional (argument textArgument (metavar "NAME"))
  <*> optional (argument textArgument (metavar "PATH"))

restoreParser :: Parser Command
restoreParser = CmdRestore <$> argument textArgument (metavar "HASH")

contextParser :: Parser Command
contextParser
  = hsubparser
    (command "add" (info contextAddParser (progDesc "Add a path or glob"))
     <> command "show" (info (pure CmdContextShow) (progDesc "Show context"))
     <> command "clear" (info (pure CmdContextClear) (progDesc "Clear context")))

contextAddParser :: Parser Command
contextAddParser = CmdContextAdd <$> argument textArgument (metavar "PATH_OR_GLOB")

helpParser :: Parser Command
helpParser = CmdHelp <$> optional (argument textArgument (metavar "COMMAND"))

textArgument :: ReadM Text
textArgument = Text.pack <$> str

replName :: String
replName = "repl"
