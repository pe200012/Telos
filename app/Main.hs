module Main ( main ) where

import           Config                   ( Config, loadConfig )

import           Control.Monad.IO.Class   ( liftIO )

import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )

import           LLM.Http                 ( runLLMHttp )

import           Polysemy                 ( runM )
import           Polysemy.Input           ( runInputConst )

import           System.Console.Haskeline ( InputT
                                          , defaultSettings
                                          , getInputLine
                                          , outputStrLn
                                          , runInputT
                                          )

main :: IO ()
main = do
  loaded <- loadConfig
  case loaded of
    Left err  -> putStrLn (Text.unpack err)
    Right cfg -> runInputT defaultSettings (loop cfg)
  where
    loop :: Config -> InputT IO ()
    loop cfg = do
      minput <- getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          response <- liftIO $ runM $ runInputConst cfg $ runLLMHttp $ askLLM (Text.pack input)
          if Text.null response
            then pure ()
            else outputStrLn (Text.unpack response)
          loop cfg
