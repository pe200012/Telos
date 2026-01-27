{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Snapshot ( Snapshot(..), saveSnapshot, loadSnapshot ) where

import           Polysemy   ( makeSem )

import           Types.Chat ( Message )

type ChatHistory = [ Message ]

data Snapshot m a where
  SaveSnapshot :: ChatHistory -> Snapshot m ()
  LoadSnapshot :: Snapshot m (Maybe ChatHistory)

makeSem ''Snapshot
