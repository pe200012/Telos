{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module FileSystem.Local ( runFileSystemLocal ) where

import           Control.Exception  ( IOException, catch, throwIO )
import           Control.Monad      ( forM )

import qualified Data.ByteString    as BS
import           Data.List          ( isPrefixOf, sort )
import           Data.Text          ( Text )
import qualified Data.Text.Encoding as Text

import           Effects.FileSystem ( FileSystem(..) )

import           Polysemy           ( Embed, Member, Sem, embed, interpret )

import           System.Directory   ( canonicalizePath
                                    , doesDirectoryExist
                                    , doesFileExist
                                    , listDirectory
                                    , makeAbsolute
                                    )
import           System.FilePath    ( (</>), addTrailingPathSeparator, isAbsolute, normalise )

runFileSystemLocal :: Member (Embed IO) r => FilePath -> Sem (FileSystem ': r) a -> Sem r a
runFileSystemLocal scopeRoot = interpret $ \case
  ListFiles path -> embed @IO $ listFilesLocal scopeRoot path
  ReadText path  -> embed @IO $ readTextLocal scopeRoot path

listFilesLocal :: FilePath -> FilePath -> IO [ FilePath ]
listFilesLocal scopeRoot inputPath = do
  absPath <- resolvePath scopeRoot inputPath
  fileExists <- doesFileExist absPath
  if fileExists
    then pure [ absPath ]
    else do
      dirExists <- doesDirectoryExist absPath
      if dirExists
        then go absPath
        else pure []
  where
    go dir = do
      entries <- sort <$> listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let child = dir </> entry
        dirExists <- doesDirectoryExist child
        if dirExists
          then if shouldIgnore entry
            then pure []
            else go child
          else do
            fileExists <- doesFileExist child
            if fileExists
              then pure [ child ]
              else pure []

readTextLocal :: FilePath -> FilePath -> IO (Maybe Text)
readTextLocal scopeRoot inputPath = do
  absPath <- resolvePath scopeRoot inputPath
  fileExists <- doesFileExist absPath
  if not fileExists
    then pure Nothing
    else do
      content <- BS.readFile absPath `catch` \(_ :: IOException) -> pure BS.empty
      let prefix = BS.take 4096 content
      if BS.elem 0 prefix
        then pure Nothing
        else case Text.decodeUtf8' content of
          Left _     -> pure Nothing
          Right text -> pure (Just text)

resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath scopeRoot inputPath = do
  scope <- canonicalizePath scopeRoot
  let path
        = if isAbsolute inputPath
          then inputPath
          else scope </> inputPath
  candidate <- canonicalizePath path `catch` \(_ :: IOException) -> makeAbsolute path
  let scopePrefix = addTrailingPathSeparator (normalise scope)
      normalized  = normalise candidate
  if scopePrefix `isPrefixOf` normalized || normalized == normalise scope
    then pure candidate
    else throwIO (userError "Path is outside scope.")

shouldIgnore :: FilePath -> Bool
shouldIgnore name = name `elem` ignoredDirs

ignoredDirs :: [ FilePath ]
ignoredDirs = [ ".git", ".stack-work", "dist", "dist-newstyle", "node_modules", ".cache" ]
