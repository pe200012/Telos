{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.LLM ( LLM(..), askLLM ) where

import           Data.Text ( Text )

import           Polysemy  ( makeSem )

data LLM m a where
  -- | Send a prompt and receive a text response.
  AskLLM :: Text -> LLM m Text

makeSem ''LLM