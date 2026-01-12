module Telos.Agent.Interrupt
  ( withInterruptHandler
  , checkInterrupted
  , signalInterrupt
  , clearInterrupt
  , InterruptHandler
  , installInterruptHandler
  , removeInterruptHandler
  ) where

import           Control.Concurrent.MVar ( isEmptyMVar )
import           Control.Exception       ( bracket )

import           Control.Lens              ( (^.) )

import           Relude

import           System.Posix.Signals    ( Handler(Catch), installHandler, sigINT )

import           Telos.Agent.Context     ( AgentContext, ctxInterrupt )

-- | Opaque handle for installed interrupt handler
newtype InterruptHandler = InterruptHandler Handler

-- | Check if interrupt has been signaled
checkInterrupted :: AgentContext -> IO Bool
checkInterrupted ctx = not <$> isEmptyMVar (ctx ^. ctxInterrupt)

-- | Signal an interrupt (called by SIGINT handler)
signalInterrupt :: AgentContext -> IO ()
signalInterrupt ctx = void $ tryPutMVar (ctx ^. ctxInterrupt) ()

-- | Clear the interrupt signal (for reuse)
clearInterrupt :: AgentContext -> IO ()
clearInterrupt ctx = void $ tryTakeMVar (ctx ^. ctxInterrupt)

-- | Install interrupt handler, returns previous handler
installInterruptHandler :: AgentContext -> IO InterruptHandler
installInterruptHandler ctx = do
  oldHandler <- installHandler sigINT (Catch $ signalInterrupt ctx) Nothing
  pure $ InterruptHandler oldHandler

-- | Remove interrupt handler, restore previous
removeInterruptHandler :: InterruptHandler -> IO ()
removeInterruptHandler (InterruptHandler prev) = void $ installHandler sigINT prev Nothing

-- | Run an action with interrupt handling
-- Installs SIGINT handler before, restores after
withInterruptHandler :: AgentContext -> IO a -> IO a
withInterruptHandler ctx action
  = bracket (installInterruptHandler ctx) removeInterruptHandler (const action)
