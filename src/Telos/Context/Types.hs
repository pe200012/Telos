{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for Dynamic Context Pruning (DCP)
module Telos.Context.Types
  ( -- * Tool Result Identification
    ToolResultId(..)
  , unToolResultId
    -- * Pruning Reasons
  , PruneReason(..)
    -- * Tool Cache
  , ToolCacheEntry(..)
  , tceToolName
  , tceParams
  , tceParamKey
  , tceTurnIndex
  , tceIsError
  , tceFilePath
  , tceTokenEstimate
    -- * Prunable Info (for LLM display)
  , PrunableInfo(..)
  , piId
  , piToolName
  , piParams
  , piTokens
  , piTurnAge
    -- * Prune State
  , PruneState(..)
  , psMarkedIds
  , psToolCache
  , psDistillations
  , psCurrentTurn
  , psStats
  , emptyPruneState
    -- * Statistics
  , PruneStats(..)
  , psTotalPruned
  , psTokensSaved
  , psByStrategy
  , emptyPruneStats
    -- * Configuration
  , PruneConfig(..)
  , pcEnabled
  , pcTurnProtection
  , pcProtectedTools
  , pcErrorThreshold
  , defaultPruneConfig
  ) where

import           Data.Aeson      ( FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value )
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Lens.Micro.TH   ( makeLenses )

import           Relude

-- | Unique identifier for a tool result in conversation
newtype ToolResultId = ToolResultId { _unToolResultId :: Int }
  deriving stock ( Eq, Ord, Show, Generic )
  deriving newtype ( FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable )

unToolResultId :: ToolResultId -> Int
unToolResultId = _unToolResultId

-- | Reason for pruning a tool result
data PruneReason
  = PruneDuplicate      -- ^ Duplicate tool call, newer exists
  | PruneSuperseded     -- ^ Write superseded by subsequent read
  | PruneError          -- ^ Error output older than threshold
  | PruneManualDiscard  -- ^ LLM explicitly discarded
  | PruneManualExtract  -- ^ LLM extracted and discarded
  deriving stock ( Eq, Show, Generic )

instance FromJSON PruneReason
instance ToJSON PruneReason

-- | Cached metadata about a tool result
data ToolCacheEntry = ToolCacheEntry
  { _tceToolName     :: Text           -- ^ Tool name (e.g., "read", "bash")
  , _tceParams       :: Value          -- ^ Original parameters
  , _tceParamKey     :: Text           -- ^ Normalized key for dedup (tool:param_summary)
  , _tceTurnIndex    :: Int            -- ^ Which turn this was in
  , _tceIsError      :: Bool           -- ^ Whether this was an error result
  , _tceFilePath     :: Maybe FilePath -- ^ For file operations
  , _tceTokenEstimate :: Int           -- ^ Estimated token count
  } deriving stock ( Eq, Show, Generic )

makeLenses ''ToolCacheEntry

instance FromJSON ToolCacheEntry
instance ToJSON ToolCacheEntry

-- | Information about a prunable tool result (for LLM display)
data PrunableInfo = PrunableInfo
  { _piId       :: ToolResultId  -- ^ Unique ID
  , _piToolName :: Text          -- ^ Tool name
  , _piParams   :: Text          -- ^ Summary of params (e.g., file path)
  , _piTokens   :: Int           -- ^ Estimated tokens
  , _piTurnAge  :: Int           -- ^ How many turns ago
  } deriving stock ( Eq, Show, Generic )

makeLenses ''PrunableInfo

instance FromJSON PrunableInfo
instance ToJSON PrunableInfo

-- | Statistics about pruning
data PruneStats = PruneStats
  { _psTotalPruned  :: Int              -- ^ Total items pruned
  , _psTokensSaved  :: Int              -- ^ Estimated tokens saved
  , _psByStrategy   :: Map Text Int     -- ^ Count per strategy
  } deriving stock ( Eq, Show, Generic )

makeLenses ''PruneStats

instance FromJSON PruneStats
instance ToJSON PruneStats

emptyPruneStats :: PruneStats
emptyPruneStats = PruneStats
  { _psTotalPruned = 0
  , _psTokensSaved = 0
  , _psByStrategy = Map.empty
  }

-- | Pruning state per session
data PruneState = PruneState
  { _psMarkedIds     :: Set ToolResultId              -- ^ IDs marked for pruning
  , _psToolCache     :: Map ToolResultId ToolCacheEntry -- ^ Metadata cache
  , _psDistillations :: Map ToolResultId Text         -- ^ Extracted summaries
  , _psCurrentTurn   :: Int                           -- ^ Current turn number
  , _psStats         :: PruneStats                    -- ^ Statistics
  } deriving stock ( Eq, Show, Generic )

makeLenses ''PruneState

instance FromJSON PruneState
instance ToJSON PruneState

emptyPruneState :: PruneState
emptyPruneState = PruneState
  { _psMarkedIds = Set.empty
  , _psToolCache = Map.empty
  , _psDistillations = Map.empty
  , _psCurrentTurn = 0
  , _psStats = emptyPruneStats
  }

-- | Configuration for pruning behavior
data PruneConfig = PruneConfig
  { _pcEnabled        :: Bool      -- ^ Whether DCP is enabled
  , _pcTurnProtection :: Int       -- ^ Don't auto-prune last N turns
  , _pcProtectedTools :: [Text]    -- ^ Never auto-prune these tools
  , _pcErrorThreshold :: Int       -- ^ Turns before error pruning
  } deriving stock ( Eq, Show, Generic )

makeLenses ''PruneConfig

instance FromJSON PruneConfig
instance ToJSON PruneConfig

defaultPruneConfig :: PruneConfig
defaultPruneConfig = PruneConfig
  { _pcEnabled = True
  , _pcTurnProtection = 2
  , _pcProtectedTools = ["task"]  -- Don't prune subagent results
  , _pcErrorThreshold = 3
  }
