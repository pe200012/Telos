module Telos.Effect.Process
  ( ProcessEff(..)
  , spawnProcess
  , sendToProcess
  , readFromProcess
  , terminateProcess
  , isProcessRunning
  , ProcessHandle(..)
  ) where

import           Polysemy         ( makeSem )

import qualified System.Process   as P

import           Telos.Core.Error ( ProcessError )

data ProcessHandle
  = ProcessHandle
  { phProcess :: P.ProcessHandle, phStdin :: Handle, phStdout :: Handle, phStderr :: Handle }

data ProcessEff m a where
  SpawnProcess
    :: FilePath -> [ String ] -> Maybe FilePath -> ProcessEff m (Either ProcessError ProcessHandle)
  SendToProcess :: ProcessHandle -> ByteString -> ProcessEff m (Either ProcessError ())
  ReadFromProcess :: ProcessHandle -> ProcessEff m (Either ProcessError ByteString)
  TerminateProcess :: ProcessHandle -> ProcessEff m ()
  IsProcessRunning :: ProcessHandle -> ProcessEff m Bool

makeSem ''ProcessEff
