{-# LANGUAGE TemplateHaskell #-}

module Telos.Storage.Types
  ( SessionId(..)
  , SessionInfo(..)
  , siId
  , siTitle
  , siCreatedAt
  , siUpdatedAt
  , StoredMessage(..)
  , smIndex
  , smMessage
  , smTimestamp
  , generateSessionId
  , makeSessionInfo
  ) where

import           Data.Aeson        ( FromJSON, ToJSON )
import           Data.Time         ( UTCTime, getCurrentTime )
import qualified Data.UUID         as UUID
import           Data.UUID.V4      ( nextRandom )
import           Lens.Micro.TH     ( makeLenses )

import           Telos.Core.Types  ( Message )

newtype SessionId = SessionId { unSessionId :: Text }
  deriving stock ( Eq, Show )
  deriving newtype ( FromJSON, ToJSON )

data SessionInfo = SessionInfo
  { _siId        :: SessionId
  , _siTitle     :: Text
  , _siCreatedAt :: UTCTime
  , _siUpdatedAt :: UTCTime
  }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( FromJSON, ToJSON )

makeLenses ''SessionInfo

data StoredMessage = StoredMessage
  { _smIndex     :: Int
  , _smMessage   :: Message
  , _smTimestamp :: UTCTime
  }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( FromJSON, ToJSON )

makeLenses ''StoredMessage

generateSessionId :: IO SessionId
generateSessionId = do
  uuid <- nextRandom
  pure $ SessionId $ "ses_" <> UUID.toText uuid

makeSessionInfo :: SessionId -> Text -> IO SessionInfo
makeSessionInfo sid title = do
  now <- getCurrentTime
  pure SessionInfo
    { _siId        = sid
    , _siTitle     = title
    , _siCreatedAt = now
    , _siUpdatedAt = now
    }
