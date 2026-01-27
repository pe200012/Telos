{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.LLM ( LLM(..), askLLM ) where

import           Data.Text  ( Text )

import           Polysemy   ( makeSem )

import           Types.Chat ( Message )

data LLM m a where
  -- | Send a message and receive a text response.
  AskLLM :: Message -> LLM m Text

makeSem ''LLM
