module Telos.Tool.EditSpec ( spec ) where

import           Control.Exception ( bracket_ )

import qualified Data.Aeson        as Aeson
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Lens.Micro        ( (^.) )

import           Relude

import           System.Directory  ( doesFileExist, getTemporaryDirectory, removeFile )
import           System.FilePath   ( (</>) )

import           Telos.Tool.Edit   ( editTool )
import           Telos.Tool.Types  ( BuiltinTool(..)
                                   , ToolContext
                                   , ToolExecutorType(..)
                                   , ToolResult
                                   , markFileRead
                                   , newToolContext
                                   , trOutput
                                   , trSuccess
                                   )

import           Test.Hspec

runExecutor :: BuiltinTool -> ToolContext -> Aeson.Value -> IO ToolResult
runExecutor bt = case _btExecutor bt of { SimpleExecutor f -> f; _ -> error "Not a SimpleExecutor" }

spec :: Spec
spec = describe "EditTool" $ do
  describe "single replacement" $ do
    it "replaces exact string match" $ do
      withTempFile "hello world" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("world" :: Text)
                , "newString" Aeson..= ("universe" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` True
        content <- TIO.readFile (toString path)
        content `shouldBe` "hello universe"

    it "returns diff in output" $ do
      withTempFile "line1\nold text\nline3" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("old text" :: Text)
                , "newString" Aeson..= ("new text" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` True
        result ^. trOutput `shouldSatisfy` T.isInfixOf "-old text"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "+new text"

  describe "multiple occurrences" $ do
    it "fails when multiple matches and replaceAll is false" $ do
      withTempFile "foo bar foo baz foo" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("foo" :: Text)
                , "newString" Aeson..= ("qux" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "3 times"

    it "replaces all when replaceAll is true" $ do
      withTempFile "foo bar foo baz foo" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("foo" :: Text)
                , "newString" Aeson..= ("qux" :: Text)
                , "replaceAll" Aeson..= True
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` True
        content <- TIO.readFile (toString path)
        content `shouldBe` "qux bar qux baz qux"
        result ^. trOutput `shouldSatisfy` T.isInfixOf "3 occurrences"

  describe "string not found" $ do
    it "fails when oldString not in file" $ do
      withTempFile "hello world" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("xyz" :: Text)
                , "newString" Aeson..= ("abc" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "not found"

  describe "same old and new string" $ do
    it "fails when oldString equals newString" $ do
      withTempFile "hello world" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("hello" :: Text)
                , "newString" Aeson..= ("hello" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "must be different"

  describe "file not found" $ do
    it "fails for non-existent file" $ do
      ctx <- newToolContext
      let args
            = Aeson.object
              [ "path" Aeson..= ("/nonexistent_12345.txt" :: Text)
              , "oldString" Aeson..= ("a" :: Text)
              , "newString" Aeson..= ("b" :: Text)
              ]
      result <- runExecutor editTool ctx args
      result ^. trSuccess `shouldBe` False
      result ^. trOutput `shouldSatisfy` T.isInfixOf "not found"

  describe "read check" $ do
    it "fails if file was not read first" $ do
      withTempFile "hello world" $ \path -> do
        ctx <- newToolContext
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("hello" :: Text)
                , "newString" Aeson..= ("hi" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` False
        result ^. trOutput `shouldSatisfy` T.isInfixOf "must Read"

  describe "multiline edits" $ do
    it "handles multiline oldString" $ do
      withTempFile "line1\nline2\nline3" $ \path -> do
        ctx <- newToolContext
        markFileRead ctx (toString path) Nothing
        let args
              = Aeson.object
                [ "path" Aeson..= path
                , "oldString" Aeson..= ("line1\nline2" :: Text)
                , "newString" Aeson..= ("replaced" :: Text)
                ]
        result <- runExecutor editTool ctx args
        result ^. trSuccess `shouldBe` True
        content <- TIO.readFile (toString path)
        content `shouldBe` "replaced\nline3"

  describe "argument validation" $ do
    it "fails with missing required fields" $ do
      ctx <- newToolContext
      let args = Aeson.object [ "path" Aeson..= ("/tmp/test.txt" :: Text) ]
      result <- runExecutor editTool ctx args
      result ^. trSuccess `shouldBe` False

withTempFile :: Text -> (Text -> IO a) -> IO a
withTempFile content action = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "telos-test-edit.txt"
  bracket_ (TIO.writeFile path content) (safeRemove path) (action (toText path))

safeRemove :: FilePath -> IO ()
safeRemove path = do
  exists <- doesFileExist path
  when exists $ removeFile path
