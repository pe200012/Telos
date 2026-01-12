module Telos.CLI.ReplSpec ( spec ) where

import           Relude

import           Telos.CLI.Repl

import           Test.Hspec

spec :: Spec
spec = do
  describe "Repl" $ do
    describe "parseCommand" $ do
      it "parses /quit" $ do
        parseCommand "/quit" `shouldBe` CmdQuit

      it "parses /q" $ do
        parseCommand "/q" `shouldBe` CmdQuit

      it "parses /exit" $ do
        parseCommand "/exit" `shouldBe` CmdQuit

      it "parses /clear" $ do
        parseCommand "/clear" `shouldBe` CmdClear

      it "parses /tools" $ do
        parseCommand "/tools" `shouldBe` CmdTools

      it "parses /servers" $ do
        parseCommand "/servers" `shouldBe` CmdServers

      it "parses /help" $ do
        parseCommand "/help" `shouldBe` CmdHelp

      it "parses /h" $ do
        parseCommand "/h" `shouldBe` CmdHelp

      it "parses /?" $ do
        parseCommand "/?" `shouldBe` CmdHelp

      it "parses /sessions" $ do
        parseCommand "/sessions" `shouldBe` CmdSessions

      it "parses /load with id" $ do
        parseCommand "/load abc123" `shouldBe` CmdLoad "abc123"

      it "parses /save without title" $ do
        parseCommand "/save" `shouldBe` CmdSave Nothing

      it "parses /save with title" $ do
        parseCommand "/save my session" `shouldBe` CmdSave (Just "my session")

      it "parses /new" $ do
        parseCommand "/new" `shouldBe` CmdNew

      it "parses /undo without count" $ do
        parseCommand "/undo" `shouldBe` CmdUndo 1

      it "parses /undo with count" $ do
        parseCommand "/undo 3" `shouldBe` CmdUndo 3

      it "parses /redo without count" $ do
        parseCommand "/redo" `shouldBe` CmdRedo 1

      it "parses /redo with count" $ do
        parseCommand "/redo 5" `shouldBe` CmdRedo 5

      it "parses regular message" $ do
        parseCommand "Hello world" `shouldBe` CmdMessage "Hello world"

      it "is case insensitive for commands" $ do
        parseCommand "/QUIT" `shouldBe` CmdQuit
        parseCommand "/Undo" `shouldBe` CmdUndo 1

      it "handles whitespace" $ do
        parseCommand "  /quit  " `shouldBe` CmdQuit
        parseCommand "/load   session1  " `shouldBe` CmdLoad "session1"
