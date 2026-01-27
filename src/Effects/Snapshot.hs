{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Snapshot
  ( Snapshot(..)
  , SnapshotCommit
  , HasUnSnapshotCommit(..)
  , mkSnapshotCommit
  , saveSnapshot
  , loadSnapshot
  ) where

import           Control.Lens.TH ( makeFieldsNoPrefix )

import           Data.Text       ( Text )

import           Polysemy        ( makeSem )

import           Types.Chat      ( Message )

type ChatHistory = [ Message ]

newtype SnapshotCommit = SnapshotCommit { _unSnapshotCommit :: Text }
  deriving ( Eq, Show )

makeFieldsNoPrefix ''SnapshotCommit

mkSnapshotCommit :: Text -> SnapshotCommit
mkSnapshotCommit = SnapshotCommit

data Snapshot m a where
  SaveSnapshot :: ChatHistory -> Snapshot m ()
  LoadSnapshot :: SnapshotCommit -> Snapshot m (Maybe ChatHistory)

makeSem ''Snapshot
