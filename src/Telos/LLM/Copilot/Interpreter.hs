{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Copilot.Interpreter ( runLLMCopilot, CopilotEnv(..), newCopilotEnv ) where

import           Conduit

import           Control.Exception        ( SomeException, try )

import           Data.Text                ( Text )
import qualified Data.Text                as T

import           Network.HTTP.Client      ( Manager )
import           Network.HTTP.Client.TLS  ( newTlsManager )

import           Polysemy

import           Telos.Core.Error         ( LLMError(..) )
import           Telos.Core.Types
import           Telos.Effect.LLM
import           Telos.LLM.Copilot.Auth
import           Telos.LLM.Copilot.Client
import           Telos.LLM.Streaming

-- | Copilot environment containing auth and client
data CopilotEnv = CopilotEnv { ceAuth :: CopilotAuth, ceClient :: CopilotClient }

-- | Create new Copilot environment
newCopilotEnv :: Manager -> CopilotConfig -> IO CopilotEnv
newCopilotEnv mgr config = do
  auth <- newCopilotAuth mgr
  let client = newCopilotClient auth mgr config
  pure CopilotEnv { ceAuth = auth, ceClient = client }

-- | Run LLM effect with Copilot implementation
runLLMCopilot :: Member (Embed IO) r => CopilotEnv -> Sem (LLM ': r) a -> Sem r a
runLLMCopilot env = interpret $ \case
  Chat messages tools       -> embed $ do
    result <- sendChatRequest (ceClient env) messages tools
    case result of
      Left err   -> pure $ Left $ LLMNetworkError err
      Right resp -> pure $ extractAssistantMessage resp

  ChatStream messages tools -> embed $ do
    result <- sendChatRequestStream (ceClient env) messages tools
    case result of
      Left err     -> pure $ errorConduit $ LLMNetworkError err
      Right source -> pure $ source .| collectStreamResult

  GetProviderInfo           -> pure
    ProviderInfo { piName          = "GitHub Copilot"
                 , piModel         = ccModel (clConfig (ceClient env))
                 , piSupportsTools = True
                 , piMaxTokens     = ccMaxTokens (clConfig (ceClient env))
                 }

-- | Extract AssistantMessage from ChatResponse
extractAssistantMessage :: ChatResponse -> Either LLMError AssistantMessage
extractAssistantMessage resp = case chChoices resp of
  []           -> Left $ LLMParseError "No choices in response"
  (choice : _) -> case chMessage choice of
    Nothing  -> Left $ LLMParseError "No message in choice"
    Just msg -> Right msg

-- | Create a conduit that immediately yields an error
errorConduit :: LLMError -> ConduitT () StreamEvent IO StreamResult
errorConduit err = pure $ StreamFailed (T.pack $ show err)
