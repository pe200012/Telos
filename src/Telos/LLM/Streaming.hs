{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Streaming
  ( StreamCollector(..)
  , newStreamCollector
  , processStreamEvent
  , finalizeStream
  , collectStreamResult
  ) where

import           Conduit

import           Control.Monad            ( forM_ )

import           Data.Aeson               ( Value(..), eitherDecode )
import qualified Data.ByteString.Lazy     as BL
import           Data.IORef
import           Data.Map.Strict          ( Map )
import qualified Data.Map.Strict          as Map
import           Data.Maybe               ( fromMaybe )
import           Data.Text                ( Text )
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE

import           Telos.Core.Types
import           Telos.LLM.Copilot.Client ( ChatResponse(..)
                                          , Choice(..)
                                          , Delta(..)
                                          , FunctionChunk(..)
                                          , ToolCallChunk(..)
                                          )

-- | Collector state for building final result from stream
data StreamCollector
  = StreamCollector { scContent      :: IORef Text
                    , scToolCalls    :: IORef (Map Int ToolCallBuilder)
                    , scFinishReason :: IORef (Maybe Text)
                    }

-- | Builder for accumulating tool call chunks
data ToolCallBuilder
  = ToolCallBuilder { tcbId :: Maybe Text, tcbName :: Maybe Text, tcbArguments :: Text }
  deriving stock ( Show )

-- | Create new stream collector
newStreamCollector :: IO StreamCollector
newStreamCollector = StreamCollector <$> newIORef "" <*> newIORef Map.empty <*> newIORef Nothing

-- | Process a streaming chat response chunk
processStreamEvent :: StreamCollector -> ChatResponse -> IO StreamEvent
processStreamEvent collector resp = do
  let choices = chChoices resp
  case choices of
    []           -> pure Ping
    (choice : _) -> do
      -- Update finish reason if present
      forM_ (chFinishReason choice) $ \reason
        -> writeIORef (scFinishReason collector) (Just reason)

      -- Process delta if present
      case chDelta choice of
        Nothing    -> pure Ping
        Just delta -> processDelta collector delta

-- | Process a delta update
processDelta :: StreamCollector -> Delta -> IO StreamEvent
processDelta collector delta = do
  -- Handle content delta first
  case dContent delta of
    Just content
      | not (T.null content) -> do
        modifyIORef' (scContent collector) (<> content)
        pure $ ContentDelta content
    _ ->
      -- Handle tool call deltas
      case dToolCalls delta of
        Nothing          -> pure Ping
        Just []          -> pure Ping
        Just (chunk : _) -> processToolCallChunk collector chunk

-- | Process a single tool call chunk
processToolCallChunk :: StreamCollector -> ToolCallChunk -> IO StreamEvent
processToolCallChunk collector chunk = do
  let idx = tccIndex chunk

  currentMap <- readIORef (scToolCalls collector)
  let current = Map.findWithDefault (ToolCallBuilder Nothing Nothing "") idx currentMap

  -- Update builder with new data
  let newId = firstJust (tccId chunk) (tcbId current)
  let newName = firstJust (tccFunction chunk >>= fcName) (tcbName current)
  let newArgs = tcbArguments current <> maybe "" id (tccFunction chunk >>= fcArguments)

  let updated = ToolCallBuilder newId newName newArgs
  writeIORef (scToolCalls collector) (Map.insert idx updated currentMap)

  -- Generate event based on what's new
  case ( tccId chunk, tccFunction chunk >>= fcName ) of
    ( Just tcId, Just name ) -> pure $ ToolCallStart idx tcId name
    _ -> case tccFunction chunk >>= fcArguments of
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
  content <- readIORef (scContent collector)
  toolCallsMap <- readIORef (scToolCalls collector)

  let toolCalls = map builderToToolCall $ Map.toList toolCallsMap

  let msg
        = AssistantMessage { amContent   = if T.null content
                               then Nothing
                               else Just content
                           , amToolCalls = toolCalls
                           }

  pure $ StreamCompleted msg

-- | Convert builder to ToolCall
builderToToolCall :: ( Int, ToolCallBuilder ) -> ToolCall
builderToToolCall ( _, builder )
  = ToolCall { tcId        = fromMaybe "" (tcbId builder)
             , tcName      = fromMaybe "" (tcbName builder)
             , tcArguments = parseArguments (tcbArguments builder)
             }
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
