module Main ( main ) where

import           Control.Exception       ( SomeException, bracket, try )
import           Control.Lens            ( view )
import           Control.Monad           ( void )

import           Crypto.Hash             ( Digest, SHA256, hash )

import           Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8   as BS8
import           Data.List               ( sortOn )
import           Data.Text               ( Text )
import qualified Data.Text               as Text
import           Data.Text.Encoding      ( encodeUtf8 )

import           Snapshot.Git
  ( HasProjectName(..)
  , HasProjectPath(..)
  , ProjectEntry
  , createProject
  , listProjects
  , projectName
  , projectPath
  , renameProject
  )

import           System.Directory        ( createDirectoryIfMissing
                                         , getTemporaryDirectory
                                         , removeDirectoryRecursive
                                         )
import           System.Environment      ( lookupEnv, setEnv, unsetEnv )
import           System.Exit             ( exitFailure )
import           System.FilePath         ( (</>), takeDirectory )
import           System.IO.Temp          ( createTempDirectory )

main :: IO ()
main = do
  ok1 <- testInvalidProjectIndexThrows
  ok2 <- testListProjectsReadsIndex
  ok3 <- testCreateProjectRejectsDuplicate
  ok4 <- testRenameProject
  if and [ ok1, ok2, ok3, ok4 ]
    then putStrLn "All tests passed"
    else exitFailure

testInvalidProjectIndexThrows :: IO Bool
testInvalidProjectIndexThrows = withTempCache $ \tempDir -> do
  let scopeRoot = "/tmp/telos-project"
      path      = indexPathFor tempDir scopeRoot
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path "{ invalid json"
  result <- try (void (listProjects scopeRoot)) :: IO (Either SomeException ())
  case result of
    Left _  -> pure True
    Right _ -> do
      putStrLn "Expected invalid project-index.json to raise an exception"
      pure False

testListProjectsReadsIndex :: IO Bool
testListProjectsReadsIndex = withTempCache $ \tempDir -> do
  let scopeRoot = "/tmp/telos-project"
      path      = indexPathFor tempDir scopeRoot
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path indexJson
  entries <- listProjects scopeRoot
  let names = sortOn id (map (view projectName) entries)
  if names == sortOn id expectedNames
    then pure True
    else do
      putStrLn ("Expected names " <> show expectedNames <> ", got " <> show names)
      pure False
  where
    expectedNames = map Text.pack [ "demo", "work" ]

    indexJson
      = "{\n"
      <> "  \"projects\": {\n"
      <> "    \"demo\": { \"uuid\": \"id-a\", \"path\": \"/tmp/proj-a\", \"lastSession\": \"hash-a\" },\n"
      <> "    \"work\": { \"uuid\": \"id-b\", \"path\": \"/tmp/proj-b\", \"lastSession\": null }\n"
      <> "  }\n"
      <> "}\n"

testCreateProjectRejectsDuplicate :: IO Bool
testCreateProjectRejectsDuplicate = withTempCache $ \_ -> do
  let scopeRoot = "/tmp/telos-project"
  first <- createProject scopeRoot "demo" "/tmp/proj"
  second <- createProject scopeRoot "demo" "/tmp/proj"
  case ( first, second ) of
    ( Right _, Left _ ) -> pure True
    _ -> do
      putStrLn "Expected duplicate project name to be rejected"
      pure False

testRenameProject :: IO Bool
testRenameProject = withTempCache $ \_ -> do
  let scopeRoot = "/tmp/telos-project"
  created <- createProject scopeRoot "untitled-1" "/tmp/proj"
  case created of
    Left err -> do
      putStrLn ("Unexpected create failure: " <> Text.unpack err)
      pure False
    Right _  -> do
      renamed <- renameProject scopeRoot "untitled-1" "demo"
      case renamed of
        Left err -> do
          putStrLn ("Unexpected rename failure: " <> Text.unpack err)
          pure False
        Right _  -> do
          entries <- listProjects scopeRoot
          let names = map (view projectName) entries
          if Text.pack "demo" `elem` names && Text.pack "untitled-1" `notElem` names
            then pure True
            else do
              putStrLn "Expected rename to update project name"
              pure False

indexPathFor :: FilePath -> FilePath -> FilePath
indexPathFor cacheRoot scopeRoot
  = cacheRoot </> "telos" </> "project-index" </> Text.unpack (sha256Hex (Text.pack scopeRoot))
  <> ".json"

sha256Hex :: Text -> Text
sha256Hex payload
  = let
      digest   = hash (encodeUtf8 payload) :: Digest SHA256
      hexBytes = convertToBase Base16 digest
    in 
      Text.pack (BS8.unpack hexBytes)

withTempCache :: (FilePath -> IO Bool) -> IO Bool
withTempCache action = do
  oldCache <- lookupEnv "XDG_CACHE_HOME"
  tmpBase <- getTemporaryDirectory
  bracket (createTempDirectory tmpBase "telos-test-") (cleanup oldCache) $ \tempDir -> do
    setEnv "XDG_CACHE_HOME" tempDir
    action tempDir
  where
    cleanup oldCache tempDir = do
      removeDirectoryRecursive tempDir
      case oldCache of
        Just value -> setEnv "XDG_CACHE_HOME" value
        Nothing    -> unsetEnv "XDG_CACHE_HOME"
