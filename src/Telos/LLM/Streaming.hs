{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.LLM.Streaming
  ( StreamCollector(..)
  , scContent
  , scToolCalls
  , scFinishReason
  , ToolCallBuilder(..)
  , tcbId
  , tcbName
  , tcbArguments
  , newStreamCollector
  , processStreamEvent
  , finalizeStream
  , collectStreamResult
  ) where

import           Conduit

import           Control.Lens             ( (?~), (^.), at, makeLenses, non )

import           Data.Aeson               ( Value(..), eitherDecode )
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE

import           Relude

import           Telos.Core.Types
import           Telos.LLM.Copilot.Client ( ChatResponse(..)
                                          , Delta(..)
                                          , ToolCallChunk(..)
                                          , chChoices
                                          , chDelta
                                          , chFinishReason
                                          , dContent
                                          , dToolCalls
                                          , fcArguments
                                          , fcName
                                          , tccFunction
                                          , tccId
                                          , tccIndex
                                          )

-- | Builder for accumulating tool call chunks (defined first since StreamCollector uses it)
data ToolCallBuilder
  = ToolCallBuilder { _tcbId :: Maybe Text, _tcbName :: Maybe Text, _tcbArguments :: Text }
  deriving stock ( Show, Eq )

makeLenses ''ToolCallBuilder

-- | Collector state for building final result from stream
data StreamCollector
  = StreamCollector { _scContent      :: IORef Text
                    , _scToolCalls    :: IORef (Map Int ToolCallBuilder)
                    , _scFinishReason :: IORef (Maybe Text)
                    }

makeLenses ''StreamCollector

-- | Create new stream collector
newStreamCollector :: IO StreamCollector
newStreamCollector = StreamCollector <$> newIORef "" <*> newIORef Map.empty <*> newIORef Nothing

-- | Process a streaming chat response chunk
processStreamEvent :: StreamCollector -> ChatResponse -> IO StreamEvent
processStreamEvent collector resp = do
  let choices = resp ^. chChoices
  case choices of
    []           -> pure Ping
    (choice : _) -> do
      -- Update finish reason if present
      forM_ (choice ^. chFinishReason) $ \reason
        -> writeIORef (collector ^. scFinishReason) (Just reason)

      -- Process delta if present
      case choice ^. chDelta of
        Nothing    -> pure Ping
        Just delta -> processDelta collector delta

-- | Process a delta update
processDelta :: StreamCollector -> Delta -> IO StreamEvent
processDelta collector delta = do
  -- Handle content delta first
  case delta ^. dContent of
    Just content
      | not (T.null content) -> do
        modifyIORef' (collector ^. scContent) (<> content)
        pure $ ContentDelta content
    _ ->
      -- Handle tool call deltas
      case delta ^. dToolCalls of
        Nothing          -> pure Ping
        Just []          -> pure Ping
        Just (chunk : _) -> processToolCallChunk collector chunk

-- | Process a single tool call chunk
processToolCallChunk :: StreamCollector -> ToolCallChunk -> IO StreamEvent
processToolCallChunk collector chunk = do
  let idx = chunk ^. tccIndex

  currentMap <- readIORef (collector ^. scToolCalls)
  let current = currentMap ^. at idx . non (ToolCallBuilder Nothing Nothing "")

  -- Update builder with new data
  let newId = firstJust (chunk ^. tccId) (current ^. tcbId)
  let newName = firstJust ((chunk ^. tccFunction) >>= (^. fcName)) (current ^. tcbName)
  let newArgs
        = (current ^. tcbArguments) <> ((chunk ^. tccFunction) >>= (^. fcArguments)) ^. non ""

  let updated = ToolCallBuilder newId newName newArgs
  writeIORef (collector ^. scToolCalls) (currentMap & at idx ?~ updated)

  -- Generate event based on what's new
  case ( chunk ^. tccId, (chunk ^. tccFunction) >>= (^. fcName) ) of
    ( Just toolCallId, Just name ) -> pure $ ToolCallStart idx toolCallId name
    _ -> case (chunk ^. tccFunction) >>= (^. fcArguments) of
      Just args
        | not (T.null args) -> pure $ ToolCallDelta idx args
      _         -> pure Ping
  where
    firstJust :: Maybe a -> Maybe a -> Maybe a
    firstJust (Just x) _ = Just x
    firstJust Nothing y  = y

-- | Finalize stream and build result
finalizeStream :: StreamCollector -> IO StreamResult
finalizeStream collector = do
  content <- readIORef (collector ^. scContent)
  toolCallsMap <- readIORef (collector ^. scToolCalls)

  let toolCalls = map builderToToolCall $ Map.toList toolCallsMap

  let msg
        = makeAssistantMessage
          (if T.null content
             then Nothing
             else Just content)
          toolCalls

  pure $ StreamCompleted msg

-- | Convert builder to ToolCall
builderToToolCall :: ( Int, ToolCallBuilder ) -> ToolCall
builderToToolCall ( _, builder )
  = makeToolCall
    (builder ^. tcbId . non "")
    (builder ^. tcbName . non "")
    (parseArguments (builder ^. tcbArguments))
  where
    parseArguments :: Text -> Value
    parseArguments t
      | T.null t = Null
      | otherwise = case eitherDecode (BL.fromStrict $ TE.encodeUtf8 t) of
        Left _  -> String t  -- If not valid JSON, treat as string
        Right v -> v

-- | Conduit that collects stream and produces StreamResult
collectStreamResult :: ConduitT ChatResponse StreamEvent IO StreamResult
collectStreamResult = do
  collector <- liftIO newStreamCollector

  let loop = do
        mChunk <- await
        case mChunk of
          Nothing    -> liftIO $ finalizeStream collector
          Just chunk -> do
            event <- liftIO $ processStreamEvent collector chunk
            yield event
            loop

  loop
