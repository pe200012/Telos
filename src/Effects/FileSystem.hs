{-# LANGUAGE GADTs #-}

module Effects.FileSystem ( FileSystem(..), listFiles, readText ) where

import           Data.Text ( Text )

import           Polysemy  ( Member, Sem, send )

data FileSystem m a where
  ListFiles :: FilePath -> FileSystem m [ FilePath ]
  ReadText :: FilePath -> FileSystem m (Maybe Text)

listFiles :: Member FileSystem r => FilePath -> Sem r [ FilePath ]
listFiles = send . ListFiles

readText :: Member FileSystem r => FilePath -> Sem r (Maybe Text)
readText = send . ReadText
