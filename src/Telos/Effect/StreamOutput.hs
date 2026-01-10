{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | StreamOutput effect for real-time streaming output.
-- Abstracts the output mechanism so it can be stdout, TVar, callback, etc.
module Telos.Effect.StreamOutput
  ( -- * Effect
    StreamOutput(..)
    -- * Actions
  , outputChunk
  , outputToolStart
  , outputToolEnd
  , outputNewline
  , flushOutput
  ) where

import           Polysemy

-- | Effect for streaming output during LLM response generation.
data StreamOutput m a where
  -- | Output a text chunk (content delta from LLM)
  OutputChunk :: Text -> StreamOutput m ()
  -- | Signal that a tool call is starting (displays tool name)
  OutputToolStart :: Text -> StreamOutput m ()
  -- | Signal that a tool call has ended
  OutputToolEnd :: Text -> StreamOutput m ()
  -- | Output a newline
  OutputNewline :: StreamOutput m ()
  -- | Flush the output buffer
  FlushOutput :: StreamOutput m ()

makeSem ''StreamOutput
