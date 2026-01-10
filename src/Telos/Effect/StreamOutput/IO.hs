{-# LANGUAGE OverloadedStrings #-}

-- | IO interpreter for StreamOutput effect.
-- Outputs to stdout with immediate flushing for real-time display.
module Telos.Effect.StreamOutput.IO
  ( -- * Interpreters
    runStreamOutputIO
  , runStreamOutputSilent
  ) where

import qualified Data.Text.IO              as TIO

import           Polysemy

import           Telos.Effect.StreamOutput

-- | Run StreamOutput by writing to stdout.
-- Each chunk is written immediately and flushed for real-time display.
runStreamOutputIO :: Member (Embed IO) r => Sem (StreamOutput ': r) a -> Sem r a
runStreamOutputIO = interpret $ \case
  OutputChunk text -> embed $ do
    TIO.putStr text
    hFlush stdout
  OutputToolStart toolName -> embed $ do
    TIO.putStr $ "\n[Calling: " <> toolName <> "]\n"
    hFlush stdout
  OutputToolEnd toolName -> embed $ do
    TIO.putStr $ "[Done: " <> toolName <> "]\n"
    hFlush stdout
  OutputNewline -> embed $ do
    TIO.putStrLn ""
    hFlush stdout
  FlushOutput -> embed @IO $ hFlush stdout

-- | Run StreamOutput silently (discard all output).
-- Useful for testing or when output is not needed.
runStreamOutputSilent :: Sem (StreamOutput ': r) a -> Sem r a
runStreamOutputSilent = interpret $ \case
  OutputChunk _     -> pass
  OutputToolStart _ -> pass
  OutputToolEnd _   -> pass
  OutputNewline     -> pass
  FlushOutput       -> pass
