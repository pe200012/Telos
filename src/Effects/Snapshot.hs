{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Snapshot ( Snapshot(..), SnapshotCommit(..), saveSnapshot, loadSnapshot ) where

import           Data.Text  ( Text )

import           Polysemy   ( makeSem )

import           Types.Chat ( Message )

type ChatHistory = [ Message ]

newtype SnapshotCommit = SnapshotCommit { unSnapshotCommit :: Text }
  deriving ( Eq, Show )

data Snapshot m a where
  SaveSnapshot :: ChatHistory -> Snapshot m ()
  LoadSnapshot :: SnapshotCommit -> Snapshot m (Maybe ChatHistory)

makeSem ''Snapshot
