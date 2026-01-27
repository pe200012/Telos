module Main ( main ) where

import           Control.Monad.IO.Class   ( liftIO )

import qualified Data.Text                as Text

import           Effects.LLM              ( askLLM )

import           LLM.Http                 ( runLLMHttp )

import           Polysemy                 ( runM )

import           System.Console.Haskeline ( InputT
                                          , defaultSettings
                                          , getInputLine
                                          , outputStrLn
                                          , runInputT
                                          )

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just input  -> do
          response <- liftIO $ runM $ runLLMHttp $ askLLM (Text.pack input)
          outputStrLn (Text.unpack response)
          loop
