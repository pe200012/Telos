{-# LANGUAGE TemplateHaskell #-}

module Telos.Effect.Process
  ( ProcessEff(..)
  , spawnProcess
  , sendToProcess
  , readFromProcess
  , terminateProcess
  , isProcessRunning
  , ProcessHandle(..)
  , phProcess
  , phStdin
  , phStdout
  , phStderr
  ) where

import           Lens.Micro.TH    ( makeLenses )

import           Polysemy         ( makeSem )

import qualified System.Process   as P

import           Telos.Core.Error ( ProcessError )

data ProcessHandle
  = ProcessHandle
  { _phProcess :: P.ProcessHandle, _phStdin :: Handle, _phStdout :: Handle, _phStderr :: Handle }

makeLenses ''ProcessHandle

data ProcessEff m a where
  SpawnProcess
    :: FilePath -> [ String ] -> Maybe FilePath -> ProcessEff m (Either ProcessError ProcessHandle)
  SendToProcess :: ProcessHandle -> ByteString -> ProcessEff m (Either ProcessError ())
  ReadFromProcess :: ProcessHandle -> ProcessEff m (Either ProcessError ByteString)
  TerminateProcess :: ProcessHandle -> ProcessEff m ()
  IsProcessRunning :: ProcessHandle -> ProcessEff m Bool

makeSem ''ProcessEff
