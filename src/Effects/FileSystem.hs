{-# LANGUAGE GADTs #-}

module Effects.FileSystem ( FileSystem(..), listFiles, readText, writeText ) where

import           Polysemy ( Member, Sem, send )

import           Relude

data FileSystem m a where
  ListFiles :: FilePath -> FileSystem m [ FilePath ]
  ReadText :: FilePath -> FileSystem m (Maybe Text)
  WriteText :: FilePath -> Text -> FileSystem m ()

listFiles :: Member FileSystem r => FilePath -> Sem r [ FilePath ]
listFiles = send . ListFiles

readText :: Member FileSystem r => FilePath -> Sem r (Maybe Text)
readText = send . ReadText

writeText :: Member FileSystem r => FilePath -> Text -> Sem r ()
writeText path content = send (WriteText path content)
