{-# LANGUAGE GADTs #-}

module CLI.Commands ( Command(..), parseCommand, renderHelp ) where

import qualified Data.Text           as Text

import           Options.Applicative

import           Relude

data Command
  = CmdLogs
  | CmdHistory
  | CmdSessionList
  | CmdSessionUse Text
  | CmdSessionNew (Maybe Text) (Maybe Text)
  | CmdCheckout Text
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
    (command "logs" (info (pure CmdLogs) (progDesc "List snapshot commits"))
     <> command "history" (info (pure CmdHistory) (progDesc "Show chat history"))
     <> command "session" (info sessionParser (progDesc "Session commands"))
     <> command "checkout" (info checkoutParser (progDesc "Checkout snapshot by hash"))
     <> command "context" (info contextParser (progDesc "Context commands"))
     <> command "help" (info helpParser (progDesc "Show help")))

sessionParser :: Parser Command
sessionParser
  = hsubparser
    (command "list" (info (pure CmdSessionList) (progDesc "List sessions"))
     <> command "use" (info sessionUseParser (progDesc "Switch session by name"))
     <> command "new" (info sessionNewParser (progDesc "Create a new session")))

sessionUseParser :: Parser Command
sessionUseParser
  = CmdSessionUse . stripSurroundingQuotes . Text.unwords
  <$> some (argument textArgument (metavar "NAME..."))

sessionNewParser :: Parser Command
sessionNewParser
  = CmdSessionNew <$> optional (argument textArgument (metavar "NAME"))
  <*> optional (argument textArgument (metavar "PATH"))

checkoutParser :: Parser Command
checkoutParser = CmdCheckout <$> argument textArgument (metavar "HASH")

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

stripSurroundingQuotes :: Text -> Text
stripSurroundingQuotes t
  | Text.length t >= 2 && Text.head t == '\'' && Text.last t == '\''
    = Text.drop 1 (Text.dropEnd 1 t)
  | Text.length t >= 2 && Text.head t == '"' && Text.last t == '"' = Text.drop 1 (Text.dropEnd 1 t)
  | otherwise = t

replName :: String
replName = "repl"
