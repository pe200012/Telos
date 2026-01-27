{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Snapshot ( Snapshot(..), saveSnapshot, loadSnapshot ) where

import           Data.Text  ( Text )

import           Polysemy   ( makeSem )

import           Types.Chat ( Message )

type ChatHistory = [ Message ]

data Snapshot m a where
  SaveSnapshot :: ChatHistory -> Snapshot m ()
  LoadSnapshot :: Text -> Snapshot m (Maybe ChatHistory)

makeSem ''Snapshot
