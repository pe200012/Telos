{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.Effect.Logger.IO ( runLoggerIO ) where

import qualified Data.Text.IO        as TIO

import           Polysemy            ( Embed, InterpreterFor, Member, embed, interpret )

import           Telos.Effect.Logger ( LogLevel(..), Logger(..) )

runLoggerIO :: Member (Embed IO) r => InterpreterFor Logger r
runLoggerIO = interpret $ \case
  Log' level msg -> embed $ TIO.putStrLn $ formatLog level msg
  where
    formatLog lvl m = "[" <> levelText lvl <> "] " <> m

    levelText       = \case
      Debug -> "DEBUG"
      Info  -> "INFO"
      Warn  -> "WARN"
      Error -> "ERROR"
