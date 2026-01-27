module Main ( main ) where

import           Control.Exception  ( SomeException, bracket, try )
import           Control.Monad      ( void )

import           Data.List          ( sortOn )
import qualified Data.Text          as Text

import           Snapshot.Git       ( ProjectEntry(..)
                                    , ensureProject
                                    , lastSessionHash
                                    , listProjects
                                    )

import           System.Directory   ( createDirectoryIfMissing
                                    , getTemporaryDirectory
                                    , removeDirectoryRecursive
                                    )
import           System.Environment ( lookupEnv, setEnv, unsetEnv )
import           System.Exit        ( exitFailure )
import           System.FilePath    ( (</>) )
import           System.IO.Temp     ( createTempDirectory )

main :: IO ()
main = do
  ok1 <- testInvalidProjectIndexThrows
  ok2 <- testListProjectsReadsIndex
  ok3 <- testEnsureProjectCreatesEntry
  if ok1 && ok2 && ok3
    then putStrLn "All tests passed"
    else exitFailure

testInvalidProjectIndexThrows :: IO Bool
testInvalidProjectIndexThrows = withTempCache $ \tempDir -> do
  let indexDir = tempDir </> "telos"
  createDirectoryIfMissing True indexDir
  writeFile (indexDir </> "project-index.json") "{ invalid json"
  result <- try (void (lastSessionHash "/tmp/telos-project")) :: IO (Either SomeException ())
  case result of
    Left _  -> pure True
    Right _ -> do
      putStrLn "Expected invalid project-index.json to raise an exception"
      pure False

testListProjectsReadsIndex :: IO Bool
testListProjectsReadsIndex = withTempCache $ \tempDir -> do
  let indexDir = tempDir </> "telos"
  createDirectoryIfMissing True indexDir
  writeFile (indexDir </> "project-index.json") indexJson
  entries <- listProjects
  let roots = sortOn id (map _projectRoot entries)
  if roots == sortOn id expectedRoots
    then pure True
    else do
      putStrLn ("Expected roots " <> show expectedRoots <> ", got " <> show roots)
      pure False
  where
    expectedRoots = map Text.pack [ "/tmp/proj-a", "/tmp/proj-b" ]

    indexJson
      = "{\n"
      <> "  \"projects\": {\n"
      <> "    \"/tmp/proj-a\": { \"uuid\": \"id-a\", \"lastSession\": \"hash-a\" },\n"
      <> "    \"/tmp/proj-b\": { \"uuid\": \"id-b\", \"lastSession\": null }\n"
      <> "  }\n"
      <> "}\n"

testEnsureProjectCreatesEntry :: IO Bool
testEnsureProjectCreatesEntry = withTempCache $ \_ -> do
  projectId <- ensureProject "/tmp/proj-c"
  entries <- listProjects
  let roots = map _projectRoot entries
  if Text.null projectId
    then do
      putStrLn "Expected ensureProject to return a non-empty project id"
      pure False
    else if Text.pack "/tmp/proj-c" `elem` roots
      then pure True
      else do
        putStrLn "Expected ensureProject to create entry for /tmp/proj-c"
        pure False

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
