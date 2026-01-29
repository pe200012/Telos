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

import           Polysemy        ( makeSem )

import           Relude

import           Types.Chat      ( Message )

type ChatHistory = [ Message ]

newtype SnapshotCommit = SnapshotCommit { _unSnapshotCommit :: Text }
  deriving ( Eq, Show )

makeFieldsNoPrefix ''SnapshotCommit

mkSnapshotCommit :: Text -> SnapshotCommit
mkSnapshotCommit = SnapshotCommit

data Snapshot m a where
  SaveSnapshot :: ChatHistory -> Snapshot m SnapshotCommit
  LoadSnapshot :: SnapshotCommit -> Snapshot m (Maybe ChatHistory)

makeSem ''Snapshot
