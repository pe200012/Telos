module Telos.Storage.SessionSpec ( spec ) where

import           Control.Exception     ( finally )

import qualified Data.Text             as T

import           Control.Lens            ( (^.) )

import           Relude

import           System.Directory      ( doesDirectoryExist
                                       , getTemporaryDirectory
                                       , removeDirectoryRecursive
                                       )
import           System.Environment    ( setEnv, unsetEnv )
import           System.FilePath       ( (</>) )

import           Telos.Core.Types      ( Message(..) )
import           Telos.Storage.Session
import           Telos.Storage.Types

import           Test.Hspec

spec :: Spec
spec = around_ withTempDataDir $ do
  describe "Session lifecycle" $ do
    it "creates a new session with title" $ do
      info <- createSession (Just "Test Session")
      info ^. siTitle `shouldBe` "Test Session"
      unSessionId (info ^. siId) `shouldSatisfy` T.isPrefixOf "ses_"

    it "creates a session with default title when None" $ do
      info <- createSession Nothing
      info ^. siTitle `shouldBe` "Untitled Session"

    it "retrieves created session by ID" $ do
      info <- createSession (Just "Retrieve Test")
      mRetrieved <- getSession (info ^. siId)
      mRetrieved `shouldSatisfy` isJust
      case mRetrieved of
        Just retrieved -> retrieved ^. siTitle `shouldBe` "Retrieve Test"
        Nothing        -> expectationFailure "Session should exist"

    it "returns Nothing for non-existent session" $ do
      mSession <- getSession (SessionId "ses_nonexistent")
      mSession `shouldBe` Nothing

    it "lists all sessions sorted by updated time" $ do
      _ <- createSession (Just "First")
      _ <- createSession (Just "Second")
      _ <- createSession (Just "Third")
      sessions <- listSessions
      length sessions `shouldBe` 3
      map (^. siTitle) sessions `shouldBe` [ "Third", "Second", "First" ]

    it "deletes session and its messages" $ do
      info <- createSession (Just "To Delete")
      let sid = info ^. siId
      appendMessage sid (UserMessage "Hello")
      deleteSession sid
      mDeleted <- getSession sid
      mDeleted `shouldBe` Nothing

  describe "Message operations" $ do
    it "appends and loads messages" $ do
      info <- createSession (Just "Message Test")
      let sid = info ^. siId
      appendMessage sid (UserMessage "First message")
      appendMessage sid (UserMessage "Second message")
      messages <- loadMessages sid
      length messages `shouldBe` 2

    it "returns empty list for session with no messages" $ do
      info <- createSession (Just "Empty")
      messages <- loadMessages (info ^. siId)
      messages `shouldBe` []

    it "getMessageCount returns correct count" $ do
      info <- createSession (Just "Count Test")
      let sid = info ^. siId
      appendMessage sid (UserMessage "One")
      appendMessage sid (UserMessage "Two")
      appendMessage sid (UserMessage "Three")
      count <- getMessageCount sid
      count `shouldBe` 3

    it "saveContextMessages only appends new messages" $ do
      info <- createSession (Just "Incremental Save")
      let sid      = info ^. siId
          history1 = [ UserMessage "First" ]
          history2 = [ UserMessage "First", UserMessage "Second", UserMessage "Third" ]
      saveContextMessages sid history1
      count1 <- getMessageCount sid
      count1 `shouldBe` 1

      saveContextMessages sid history2
      count2 <- getMessageCount sid
      count2 `shouldBe` 3

    it "loadContextMessages returns all messages" $ do
      info <- createSession (Just "Load Test")
      let sid = info ^. siId
      appendMessage sid (UserMessage "Hello")
      appendMessage sid (SystemMessage "System message")
      messages <- loadContextMessages sid
      length messages `shouldBe` 2

  describe "touchSession" $ do
    it "updates the session timestamp" $ do
      info <- createSession (Just "Touch Test")
      let sid          = info ^. siId
          originalTime = info ^. siUpdatedAt
      touchSession sid
      mUpdated <- getSession sid
      case mUpdated of
        Just updated -> updated ^. siUpdatedAt `shouldSatisfy` (>= originalTime)
        Nothing      -> expectationFailure "Session should exist"

withTempDataDir :: IO () -> IO ()
withTempDataDir action = do
  tmpBase <- getTemporaryDirectory
  let tmpDir = tmpBase </> "telos-test-storage"

  removeIfExists tmpDir
  setEnv "XDG_DATA_HOME" tmpDir

  action `finally` cleanup tmpDir
  where
    removeIfExists dir = do
      exists <- doesDirectoryExist dir
      when exists $ removeDirectoryRecursive dir

    cleanup tmpDir = do
      unsetEnv "XDG_DATA_HOME"
      removeIfExists tmpDir
