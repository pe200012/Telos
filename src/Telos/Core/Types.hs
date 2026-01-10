{-# LANGUAGE TemplateHaskell #-}

module Telos.Core.Types
  ( Role(..)
  , Message(..)
  -- Message Prisms
  , _UserMessage
  , _AssistantMsg
  , _SystemMessage
  , _ToolResultMessage
  , ToolCall
  , makeToolCall
  , tcId
  , tcName
  , tcArguments
  , Tool
  , makeTool
  , toolName
  , toolDescription
  , toolInputSchema
  , StreamEvent(ContentDelta, ToolCallStart, ToolCallDelta, Ping)
  -- StreamEvent Prisms
  , _ContentDelta
  , _ToolCallStart
  , _ToolCallDelta
  , _Ping
  , tcsIndex
  , tcsId
  , tcsName
  , tcdIndex
  , tcdArguments
  , StreamResult(..)
  -- StreamResult Prisms
  , _StreamCompleted
  , _StreamInterrupted
  , _StreamFailed
  , PartialMessage
  , makePartialMessage
  , pmContentSoFar
  , pmToolCallsSoFar
  , PartialToolCall
  , makePartialToolCall
  , ptcId
  , ptcName
  , ptcArgumentsSoFar
  , AssistantMessage
  , makeAssistantMessage
  , amContent
  , amToolCalls
  , ProviderInfo
  , makeProviderInfo
  , piName
  , piModel
  , piSupportsTools
  , piMaxTokens
  , umContent
  , smContent
  , trmToolCallId
  , trmToolName
  , trmResult
  , trmIsError
  ) where

import           Data.Aeson          ( (.:)
                                     , (.:?)
                                     , (.=)
                                     , FromJSON(..)
                                     , ToJSON(..)
                                     , Value
                                     , object
                                     , withObject
                                     , withText
                                     )

import           Lens.Micro          ( (^.), non )
import           Lens.Micro.Pro      ( Prism', prism' )
import           Lens.Micro.TH       ( makeLenses )

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

data ToolCall = ToolCall
  { _tcId        :: Text
  , _tcName      :: Text
  , _tcArguments :: Value
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ToolCall

makeToolCall :: Text -- ^ id
             -> Text -- ^ name
             -> Value -- ^ arguments
             -> ToolCall
makeToolCall id' name args =
  ToolCall { _tcId        = id'
           , _tcName      = name
           , _tcArguments = args
           }

instance ToJSON ToolCall where
  toJSON tc
    = object
      [ "id" .= (tc ^. tcId)
      , "type" .= ("function" :: Text)
      , "function" .= object [ "name" .= (tc ^. tcName), "arguments" .= (tc ^. tcArguments) ]
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> do
    tcId' <- o .: "id"
    fn <- o .: "function"
    tcName' <- fn .: "name"
    tcArguments' <- fn .: "arguments"
    pure ToolCall { _tcId = tcId', _tcName = tcName', _tcArguments = tcArguments' }

data Tool = Tool
  { _toolName        :: Text
  , _toolDescription :: Maybe Text
  , _toolInputSchema :: Value
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''Tool

makeTool :: Text -> Value -> Tool
makeTool name schema = Tool
  { _toolName = name
  , _toolDescription = Nothing
  , _toolInputSchema = schema
  }

instance ToJSON Tool where
  toJSON t
    = object
      [ "type" .= ("function" :: Text)
      , "function"
        .= object
          [ "name" .= (t ^. toolName)
          , "description" .= (t ^. toolDescription)
          , "parameters" .= (t ^. toolInputSchema)
          ]
      ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> do
    fn <- o .: "function"
    toolName' <- fn .: "name"
    toolDescription' <- fn .:? "description"
    toolInputSchema' <- fn .: "parameters"
    pure
      Tool { _toolName        = toolName'
           , _toolDescription = toolDescription'
           , _toolInputSchema = toolInputSchema'
           }

data AssistantMessage = AssistantMessage
  { _amContent   :: Maybe Text
  , _amToolCalls :: [ ToolCall ]
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''AssistantMessage

makeAssistantMessage :: Maybe Text -> [ToolCall] -> AssistantMessage
makeAssistantMessage content tcs = AssistantMessage
  { _amContent = content
  , _amToolCalls = tcs
  }

instance ToJSON AssistantMessage where
  toJSON am
    = object
      [ "role" .= Assistant
      , "content" .= (am ^. amContent)
      , "tool_calls"
        .= if null (am ^. amToolCalls)
          then Nothing
          else Just (am ^. amToolCalls)
      ]

instance FromJSON AssistantMessage where
  parseJSON = withObject "AssistantMessage" $ \o -> do
    amContent' <- o .:? "content"
    amToolCalls' <- o .:? "tool_calls"
    pure AssistantMessage { _amContent = amContent', _amToolCalls = amToolCalls' ^. non [] }

data Message
  = UserMessage { _umContent :: Text }
  | AssistantMsg AssistantMessage
  | SystemMessage { _smContent :: Text }
  | ToolResultMessage
    { _trmToolCallId :: Text
    , _trmToolName   :: Text
    , _trmResult     :: Text
    , _trmIsError    :: Bool
    }
  deriving stock ( Eq, Show, Generic )

makeLenses ''Message

-- | Prisms for Message sum type
_UserMessage :: Prism' Message Text
_UserMessage = prism' UserMessage $ \case
  UserMessage c -> Just c
  _             -> Nothing

_AssistantMsg :: Prism' Message AssistantMessage
_AssistantMsg = prism' AssistantMsg $ \case
  AssistantMsg am -> Just am
  _               -> Nothing

_SystemMessage :: Prism' Message Text
_SystemMessage = prism' SystemMessage $ \case
  SystemMessage c -> Just c
  _               -> Nothing

_ToolResultMessage :: Prism' Message (Text, Text, Text, Bool)
_ToolResultMessage = prism' (\(cid, n, r, e) -> ToolResultMessage cid n r e) $ \case
  ToolResultMessage cid n r e -> Just (cid, n, r, e)
  _                           -> Nothing

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
  | ToolCallStart { _tcsIndex :: Int, _tcsId :: Text, _tcsName :: Text }
  | ToolCallDelta { _tcdIndex :: Int, _tcdArguments :: Text }
  | Ping
  deriving stock ( Eq, Show )

makeLenses ''StreamEvent

-- | Prisms for StreamEvent sum type
_ContentDelta :: Prism' StreamEvent Text
_ContentDelta = prism' ContentDelta $ \case
  ContentDelta t -> Just t
  _              -> Nothing

_ToolCallStart :: Prism' StreamEvent (Int, Text, Text)
_ToolCallStart = prism' (\(i, tid, n) -> ToolCallStart i tid n) $ \case
  ToolCallStart i tid n -> Just (i, tid, n)
  _                     -> Nothing

_ToolCallDelta :: Prism' StreamEvent (Int, Text)
_ToolCallDelta = prism' (\(i, args) -> ToolCallDelta i args) $ \case
  ToolCallDelta i args -> Just (i, args)
  _                    -> Nothing

_Ping :: Prism' StreamEvent ()
_Ping = prism' (const Ping) $ \case
  Ping -> Just ()
  _    -> Nothing

data PartialToolCall = PartialToolCall
  { _ptcId             :: Maybe Text
  , _ptcName           :: Maybe Text
  , _ptcArgumentsSoFar :: Text
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''PartialToolCall

makePartialToolCall :: Text -- ^ id
                    -> Text -- ^ name
                    -> Text       -- ^ arguments
                    -> PartialToolCall
makePartialToolCall id' name args =
  PartialToolCall { _ptcId = Just id'
                  , _ptcName = Just name
                  , _ptcArgumentsSoFar = args }

data PartialMessage = PartialMessage
  { _pmContentSoFar   :: Text
  , _pmToolCallsSoFar :: [ PartialToolCall ]
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''PartialMessage

makePartialMessage :: Text -> [PartialToolCall] -> PartialMessage
makePartialMessage content tcs = PartialMessage
  { _pmContentSoFar = content
  , _pmToolCallsSoFar = tcs
  }

data StreamResult
  = StreamCompleted AssistantMessage
  | StreamInterrupted PartialMessage
  | StreamFailed Text
  deriving stock ( Eq, Show )

-- | Prisms for StreamResult sum type
_StreamCompleted :: Prism' StreamResult AssistantMessage
_StreamCompleted = prism' StreamCompleted $ \case
  StreamCompleted am -> Just am
  _                  -> Nothing

_StreamInterrupted :: Prism' StreamResult PartialMessage
_StreamInterrupted = prism' StreamInterrupted $ \case
  StreamInterrupted pm -> Just pm
  _                    -> Nothing

_StreamFailed :: Prism' StreamResult Text
_StreamFailed = prism' StreamFailed $ \case
  StreamFailed t -> Just t
  _              -> Nothing

data ProviderInfo = ProviderInfo
  { _piName          :: Text
  , _piModel         :: Text
  , _piSupportsTools :: Bool
  , _piMaxTokens     :: Maybe Int
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''ProviderInfo

makeProviderInfo :: Text -> Text -> ProviderInfo
makeProviderInfo name model = ProviderInfo
  { _piName = name
  , _piModel = model
  , _piSupportsTools = True
  , _piMaxTokens = Nothing
  }
