{-# LANGUAGE RecursiveDo #-}

module NewUI ( main ) where

import qualified Reactive.Banana               as Banana
import           Reactive.Banana.Frameworks    ( actuate, newAddHandler )
import           Relude
import           Telos.TUI.Chat
import           Telos.TUI.FRP
import           WEditor.LineWrap              ( breakExact )
import           WEditorBrick.WrappingEditor

-- | Main application entry point using bricki-banana pattern
main :: IO ()
main = do
  -- MVar to block main thread until application halts
  finMVar <- newEmptyMVar

  -- Initial editor state (pure, passed to FRP network)
  let initialEditor = newEditor breakExact InputField []

  -- Startup event handler - brickNetwork expects (AddHandler (), Handler ()) tuple
  startup <- newAddHandler

  -- Compile FRP network
  network <- Banana.compile $ 
    buildChatNetwork finMVar initialEditor startup initialAttrMap

  -- Start the network
  actuate network

  -- Trigger startup event to initialize
  snd startup ()

  -- Block until halt
  takeMVar finMVar
