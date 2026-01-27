module Main ( main ) where

import           CLI.Repl  ( runRepl )

import           Config    ( loadConfig )

import qualified Data.Text as Text

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> runRepl cfg
