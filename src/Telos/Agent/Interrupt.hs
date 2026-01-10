module Telos.Agent.Interrupt
  ( withInterruptHandler
  , checkInterrupted
  , signalInterrupt
  , clearInterrupt
  , InterruptHandler
  , installInterruptHandler
  , removeInterruptHandler
  ) where

import           Control.Concurrent.MVar ( MVar, isEmptyMVar, tryPutMVar, tryTakeMVar )
import           Control.Exception       ( bracket )

import           System.Posix.Signals    ( Handler(Catch), installHandler, sigINT )

import           Telos.Agent.Context     ( AgentContext(..) )

-- | Opaque handle for installed interrupt handler
newtype InterruptHandler = InterruptHandler { unHandler :: Handler }

-- | Check if interrupt has been signaled
checkInterrupted :: AgentContext -> IO Bool
checkInterrupted ctx = not <$> isEmptyMVar (ctxInterrupt ctx)

-- | Signal an interrupt (called by SIGINT handler)
signalInterrupt :: AgentContext -> IO ()
signalInterrupt ctx = do
  _ <- tryPutMVar (ctxInterrupt ctx) ()
  pure ()

-- | Clear the interrupt signal (for reuse)
clearInterrupt :: AgentContext -> IO ()
clearInterrupt ctx = do
  _ <- tryTakeMVar (ctxInterrupt ctx)
  pure ()

-- | Install interrupt handler, returns previous handler
installInterruptHandler :: AgentContext -> IO InterruptHandler
installInterruptHandler ctx = do
  oldHandler <- installHandler sigINT (Catch $ signalInterrupt ctx) Nothing
  pure $ InterruptHandler oldHandler

-- | Remove interrupt handler, restore previous
removeInterruptHandler :: InterruptHandler -> IO ()
removeInterruptHandler (InterruptHandler prev) = do
  _ <- installHandler sigINT prev Nothing
  pure ()

-- | Run an action with interrupt handling
-- Installs SIGINT handler before, restores after
withInterruptHandler :: AgentContext -> IO a -> IO a
withInterruptHandler ctx action
  = bracket (installInterruptHandler ctx) removeInterruptHandler (const action)
