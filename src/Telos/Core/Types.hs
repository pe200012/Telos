module Telos.Core.Types
  ( Role(..)
  , Message(..)
  , ToolCall(..)
  , Tool(..)
  , StreamEvent(..)
  , StreamResult(..)
  , PartialMessage(..)
  , PartialToolCall(..)
  , AssistantMessage(..)
  , ProviderInfo(..)
  ) where

import           Data.Aeson   ( (.:)
                              , (.:?)
                              , (.=)
                              , FromJSON(..)
                              , ToJSON(..)
                              , Value
                              , object
                              , withObject
                              , withText
                              )

data Role = User | Assistant | System | ToolRole
  deriving stock ( Eq, Show, Generic )

instance ToJSON Role where
  toJSON = \case
    User      -> "user"
    Assistant -> "assistant"
    System    -> "system"
    ToolRole  -> "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user"      -> pure User
    "assistant" -> pure Assistant
    "system"    -> pure System
    "tool"      -> pure ToolRole
    other       -> fail $ "Unknown role: " <> show other

data ToolCall = ToolCall { tcId :: Text, tcName :: Text, tcArguments :: Value }
  deriving stock ( Eq, Show, Generic )

instance ToJSON ToolCall where
  toJSON tc
    = object
      [ "id" .= tcId tc
      , "type" .= ("function" :: Text)
      , "function" .= object [ "name" .= tcName tc, "arguments" .= tcArguments tc ]
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> do
    tcId' <- o .: "id"
    fn <- o .: "function"
    tcName' <- fn .: "name"
    tcArguments' <- fn .: "arguments"
    pure ToolCall { tcId = tcId', tcName = tcName', tcArguments = tcArguments' }

data Tool = Tool { toolName :: Text, toolDescription :: Maybe Text, toolInputSchema :: Value }
  deriving stock ( Eq, Show, Generic )

instance ToJSON Tool where
  toJSON t
    = object
      [ "type" .= ("function" :: Text)
      , "function"
        .= object
          [ "name" .= toolName t
          , "description" .= toolDescription t
          , "parameters" .= toolInputSchema t
          ]
      ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> do
    fn <- o .: "function"
    toolName' <- fn .: "name"
    toolDescription' <- fn .:? "description"
    toolInputSchema' <- fn .: "parameters"
    pure
      Tool { toolName        = toolName'
           , toolDescription = toolDescription'
           , toolInputSchema = toolInputSchema'
           }

data AssistantMessage = AssistantMessage { amContent :: Maybe Text, amToolCalls :: [ ToolCall ] }
  deriving stock ( Eq, Show, Generic )

instance ToJSON AssistantMessage where
  toJSON am
    = object
      [ "role" .= Assistant
      , "content" .= amContent am
      , "tool_calls"
        .= if null (amToolCalls am)
          then Nothing
          else Just (amToolCalls am)
      ]

instance FromJSON AssistantMessage where
  parseJSON = withObject "AssistantMessage" $ \o -> do
    amContent' <- o .:? "content"
    amToolCalls' <- o .:? "tool_calls"
    pure AssistantMessage { amContent = amContent', amToolCalls = fromMaybe [] amToolCalls' }

data Message
  = UserMessage { umContent :: Text }
  | AssistantMsg AssistantMessage
  | SystemMessage { smContent :: Text }
  | ToolResultMessage
    { trmToolCallId :: Text, trmToolName :: Text, trmResult :: Text, trmIsError :: Bool }
  deriving stock ( Eq, Show, Generic )

instance ToJSON Message where
  toJSON = \case
    UserMessage content -> object [ "role" .= User, "content" .= content ]
    AssistantMsg am -> toJSON am
    SystemMessage content -> object [ "role" .= System, "content" .= content ]
    ToolResultMessage callId name result isErr -> object
      [ "role" .= ToolRole
      , "tool_call_id" .= callId
      , "name" .= name
      , "content" .= result
      , "is_error" .= isErr
      ]

data StreamEvent
  = ContentDelta Text
  | ToolCallStart { tcsIndex :: Int, tcsId :: Text, tcsName :: Text }
  | ToolCallDelta { tcdIndex :: Int, tcdArguments :: Text }
  | Ping
  deriving stock ( Eq, Show )

data PartialToolCall
  = PartialToolCall { ptcId :: Maybe Text, ptcName :: Maybe Text, ptcArgumentsSoFar :: Text }
  deriving stock ( Eq, Show, Generic )

data PartialMessage
  = PartialMessage { pmContentSoFar :: Text, pmToolCallsSoFar :: [ PartialToolCall ] }
  deriving stock ( Eq, Show, Generic )

data StreamResult
  = StreamCompleted AssistantMessage
  | StreamInterrupted PartialMessage
  | StreamFailed Text
  deriving stock ( Eq, Show )

data ProviderInfo
  = ProviderInfo
  { piName :: Text, piModel :: Text, piSupportsTools :: Bool, piMaxTokens :: Maybe Int }
  deriving stock ( Eq, Show, Generic )
