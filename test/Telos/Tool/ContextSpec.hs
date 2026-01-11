module Telos.Tool.ContextSpec ( spec ) where

import           Test.Hspec

import           Data.Time            ( getCurrentTime )
import           Lens.Micro           ( (^.) )

import           Telos.Tool.Types     ( newToolContext, markFileRead, wasFileRead
                                      , getFileReadTime, friReadTime, friModTime
                                      )

spec :: Spec
spec = describe "ToolContext" $ do
  describe "newToolContext" $ do
    it "creates empty context" $ do
      ctx <- newToolContext
      wasRead <- wasFileRead ctx "/some/path"
      wasRead `shouldBe` False

  describe "markFileRead" $ do
    it "marks file as read" $ do
      ctx <- newToolContext
      markFileRead ctx "/test/file.txt" Nothing
      wasRead <- wasFileRead ctx "/test/file.txt"
      wasRead `shouldBe` True

    it "records read time" $ do
      ctx <- newToolContext
      timeBefore <- getCurrentTime
      markFileRead ctx "/test/file.txt" Nothing
      timeAfter <- getCurrentTime
      mInfo <- getFileReadTime ctx "/test/file.txt"
      case mInfo of
        Nothing -> expectationFailure "Expected file read info"
        Just info -> do
          (info ^. friReadTime) `shouldSatisfy` (>= timeBefore)
          (info ^. friReadTime) `shouldSatisfy` (<= timeAfter)

    it "records modification time" $ do
      ctx <- newToolContext
      now <- getCurrentTime
      markFileRead ctx "/test/file.txt" (Just now)
      mInfo <- getFileReadTime ctx "/test/file.txt"
      case mInfo of
        Nothing -> expectationFailure "Expected file read info"
        Just info -> info ^. friModTime `shouldBe` Just now

  describe "wasFileRead" $ do
    it "returns False for unread files" $ do
      ctx <- newToolContext
      markFileRead ctx "/file1.txt" Nothing
      wasRead <- wasFileRead ctx "/file2.txt"
      wasRead `shouldBe` False

    it "returns True for read files" $ do
      ctx <- newToolContext
      markFileRead ctx "/file1.txt" Nothing
      wasRead <- wasFileRead ctx "/file1.txt"
      wasRead `shouldBe` True

  describe "multiple files" $ do
    it "tracks multiple files independently" $ do
      ctx <- newToolContext
      markFileRead ctx "/file1.txt" Nothing
      markFileRead ctx "/file2.txt" Nothing
      
      wasRead1 <- wasFileRead ctx "/file1.txt"
      wasRead2 <- wasFileRead ctx "/file2.txt"
      wasRead3 <- wasFileRead ctx "/file3.txt"
      
      wasRead1 `shouldBe` True
      wasRead2 `shouldBe` True
      wasRead3 `shouldBe` False

  describe "getFileReadTime" $ do
    it "returns Nothing for unread files" $ do
      ctx <- newToolContext
      mInfo <- getFileReadTime ctx "/unread.txt"
      mInfo `shouldBe` Nothing

    it "returns Just for read files" $ do
      ctx <- newToolContext
      markFileRead ctx "/read.txt" Nothing
      mInfo <- getFileReadTime ctx "/read.txt"
      mInfo `shouldSatisfy` isJust
