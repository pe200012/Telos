{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Provider.Copilot (createCopilotProvider) where

import           Conduit

import           Control.Lens              ( (^.) )
import           Relude

import           Telos.Core.Error          ( LLMError(..) )
import           Telos.Core.Types          ( Message, Tool, AssistantMessage, StreamEvent )
import           Telos.LLM.Provider.Types  ( Provider(..), ProviderType(..) )
import qualified Telos.LLM.Copilot.Client  as CC

-- | Create Copilot provider wrapper
createCopilotProvider :: CC.CopilotClient -> IO Provider
createCopilotProvider client = pure $ Provider
  { providerType = Copilot
  , providerModel = client ^. CC.clConfig . CC.ccModel
  , providerComplete = copilotComplete client
  , providerCompleteStreaming = copilotCompleteStreaming client
  }

-- | Send non-streaming request via Copilot
copilotComplete :: CC.CopilotClient -> [Message] -> [Tool] -> IO (Either LLMError AssistantMessage)
copilotComplete client msgs tools = do
  result <- CC.sendChatRequest client msgs tools
  pure $ case result of
    Left err -> Left $ LLMNetworkError err
    Right chatResp ->
      case chatResp ^. CC.chChoices of
        [] -> Left $ LLMInvalidResponse "No choices in response"
        (choice:_) -> case choice ^. CC.chMessage of
          Nothing -> Left $ LLMInvalidResponse "No message in choice"
          Just assistantMsg -> Right assistantMsg

-- | Send streaming request via Copilot
copilotCompleteStreaming :: CC.CopilotClient -> [Message] -> [Tool] -> (StreamEvent -> IO ()) -> IO (ConduitT () StreamEvent IO ())
copilotCompleteStreaming client msgs tools _callback = do
  result <- CC.sendChatRequestStream client msgs tools
  pure $ case result of
    Left _ -> pure ()
    Right conduit -> conduit .| pure ()
