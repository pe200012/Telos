
module Telos.TUI.FRP ( FRPEvent(..), buildFRPNetwork ) where

import           Control.Concurrent.Chan    ( Chan )

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Relude

-- | Events from the UI layer
data FRPEvent = TextInput Text | SubmitInput
  deriving ( Eq, Show )

-- | Build FRP event network
buildFRPNetwork :: AddHandler FRPEvent
                -> Chan Text  -- Channel to receive submitted messages
                -> IO EventNetwork
buildFRPNetwork eventInput chan = compile $ do
  -- Create event from addHandler inside compile block
  event <- fromAddHandler eventInput

  -- Filter only submit events
  let submitE = filterE (== SubmitInput) event

  -- For this simple example, we're just demonstrating FRP structure.
  -- In a more complete implementation, we would:
  -- 1. Collect text input events
  -- 2. On submit, send the accumulated text through the channel
  -- 3. Use FRP behaviors to track current input state
  let _ = submitE
  let _ = chan

  -- The actual input handling will be done in the Chat module
  -- This shows the structure for future FRP-based event handling
  pure ()
