{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.LLM.Copilot.Interpreter
  ( runLLMCopilot
  , CopilotEnv(..)
  , ceAuth
  , ceClient
  , newCopilotEnv
  ) where

import           Conduit

import qualified Data.Text                as T

import           Lens.Micro               ( (.~), (^.) )
import           Lens.Micro.TH            ( makeLenses )

import           Network.HTTP.Client      ( Manager )

import           Polysemy

import           Telos.Core.Error         ( LLMError(..) )
import           Telos.Core.Types
import           Telos.Effect.LLM
import           Telos.LLM.Copilot.Auth
import           Telos.LLM.Copilot.Client
import           Telos.LLM.Streaming

-- | Copilot environment containing auth and client
data CopilotEnv = CopilotEnv { _ceAuth :: CopilotAuth, _ceClient :: CopilotClient }

makeLenses ''CopilotEnv

-- | Create new Copilot environment
newCopilotEnv :: Manager -> CopilotConfig -> IO CopilotEnv
newCopilotEnv mgr config = do
  auth <- newCopilotAuth mgr
  let client = newCopilotClient auth mgr config
  pure CopilotEnv { _ceAuth = auth, _ceClient = client }

-- | Run LLM effect with Copilot implementation
runLLMCopilot :: Member (Embed IO) r => CopilotEnv -> Sem (LLM ': r) a -> Sem r a
runLLMCopilot env = interpret $ \case
  Chat messages tools       -> embed $ do
    result <- sendChatRequest (env ^. ceClient) messages tools
    case result of
      Left err   -> pure $ Left $ LLMNetworkError err
      Right resp -> pure $ extractAssistantMessage resp

  ChatStream messages tools -> embed $ do
    result <- sendChatRequestStream (env ^. ceClient) messages tools
    case result of
      Left err     -> pure $ errorConduit $ LLMNetworkError err
      Right source -> pure $ source .| collectStreamResult

  GetProviderInfo           -> pure
    $ makeProviderInfo "GitHub Copilot" (env ^. ceClient . clConfig . ccModel)
      & piMaxTokens .~ (env ^. ceClient . clConfig . ccMaxTokens)

-- | Extract AssistantMessage from ChatResponse
extractAssistantMessage :: ChatResponse -> Either LLMError AssistantMessage
extractAssistantMessage resp = case resp ^. chChoices of
  []           -> Left $ LLMParseError "No choices in response"
  (choice : _) -> case choice ^. chMessage of
    Nothing  -> Left $ LLMParseError "No message in choice"
    Just msg -> Right msg

-- | Create a conduit that immediately yields an error
errorConduit :: LLMError -> ConduitT () StreamEvent IO StreamResult
errorConduit err = pure $ StreamFailed (T.pack $ show err)
