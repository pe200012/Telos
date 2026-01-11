module Telos.Effect.Logger
  ( Logger(..)
  , LogLevel(..)
  , log'
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import           Polysemy ( Member, Sem, makeSem )

import           Relude

data LogLevel = Debug | Info | Warn | Error
  deriving stock ( Eq, Ord, Show )

data Logger m a where
  Log' :: LogLevel -> Text -> Logger m ()

makeSem ''Logger

logDebug :: Member Logger r => Text -> Sem r ()
logDebug = log' Debug

logInfo :: Member Logger r => Text -> Sem r ()
logInfo = log' Info

logWarn :: Member Logger r => Text -> Sem r ()
logWarn = log' Warn

logError :: Member Logger r => Text -> Sem r ()
logError = log' Error
