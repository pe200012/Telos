module Main ( main ) where

import           Control.Exception  ( SomeException, bracket, try )
import           Control.Monad      ( void )

import           Snapshot.Git       ( lastSessionHash )

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
  ok <- testInvalidProjectIndexThrows
  if ok
    then putStrLn "All tests passed"
    else exitFailure

testInvalidProjectIndexThrows :: IO Bool
testInvalidProjectIndexThrows = do
  oldCache <- lookupEnv "XDG_CACHE_HOME"
  tmpBase <- getTemporaryDirectory
  bracket (createTempDirectory tmpBase "telos-test-") (cleanup oldCache) $ \tempDir -> do
    setEnv "XDG_CACHE_HOME" tempDir
    let indexDir = tempDir </> "telos"
    createDirectoryIfMissing True indexDir
    writeFile (indexDir </> "project-index.json") "{ invalid json"
    result <- try (void (lastSessionHash "/tmp/telos-project")) :: IO (Either SomeException ())
    case result of
      Left _  -> pure True
      Right _ -> do
        putStrLn "Expected invalid project-index.json to raise an exception"
        pure False
  where
    cleanup oldCache tempDir = do
      removeDirectoryRecursive tempDir
      case oldCache of
        Just value -> setEnv "XDG_CACHE_HOME" value
        Nothing    -> unsetEnv "XDG_CACHE_HOME"
