module Telos.Effect.LLM ( LLM(..), chat, chatStream, getProviderInfo ) where

import           Conduit          ( ConduitT )

import           Data.Text        ( Text )

import           Polysemy         ( Sem, makeSem )

import           Telos.Core.Error ( LLMError )
import           Telos.Core.Types ( AssistantMessage
                                  , Message
                                  , ProviderInfo
                                  , StreamEvent
                                  , StreamResult
                                  , Tool
                                  )

data LLM m a where
  Chat :: [ Message ] -> [ Tool ] -> LLM m (Either LLMError AssistantMessage)
  ChatStream :: [ Message ] -> [ Tool ] -> LLM m (ConduitT () StreamEvent IO StreamResult)
  GetProviderInfo :: LLM m ProviderInfo

makeSem ''LLM
