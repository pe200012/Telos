{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.LLM ( LLM(..), askLLM ) where

import           Polysemy   ( makeSem )

import           Relude

import           Types.Chat ( Message )

data LLM m a where
  -- | Send a message and receive a text response.
  AskLLM :: Message -> LLM m Text

makeSem ''LLM
