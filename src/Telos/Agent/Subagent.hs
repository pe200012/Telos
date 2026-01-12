{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Subagent
  ( SubagentConfig(..)
  , sacPrompt
  , sacMaxIterations
  , sacMaxDepth
  , sacCurrentDepth
  , SubagentResult(..)
  , createSubagentContext
  , defaultSubagentConfig
  , runSubagent
  ) where

import           Lens.Micro       ( (^.), (.~) )
import           Lens.Micro.TH    ( makeLenses )

import           Polysemy         ( Embed, Members, Sem, embed )

import           Relude

import           Telos.Agent.Config  ( acMaxIterations )
import           Telos.Agent.Context ( AgentContext(..)
                                     , ctxConfig
                                     , ctxInterrupt
                                     , ctxPruneState
                                     , ctxToolContext
                                     , ctxTools
                                     )

-- | Configuration for spawning a subagent
data SubagentConfig = SubagentConfig
  { _sacPrompt        :: Text  -- ^ Task prompt for the subagent
  , _sacMaxIterations :: Int   -- ^ Maximum iterations (default: 10)
  , _sacMaxDepth      :: Int   -- ^ Maximum nesting depth (default: 3)
  , _sacCurrentDepth  :: Int   -- ^ Current depth level (0 for root)
  } deriving stock (Eq, Show)

makeLenses ''SubagentConfig

-- | Result from a subagent execution
data SubagentResult
  = SubagentSuccess Text      -- ^ Successful completion with response
  | SubagentError Text        -- ^ Error during execution
  | SubagentMaxIterations     -- ^ Hit iteration limit
  | SubagentInterrupted       -- ^ Interrupted by parent
  deriving stock (Eq, Show)

-- | Create default subagent config with just a prompt
defaultSubagentConfig :: Text -> SubagentConfig
defaultSubagentConfig prompt = SubagentConfig
  { _sacPrompt        = prompt
  , _sacMaxIterations = 10
  , _sacMaxDepth      = 3
  , _sacCurrentDepth  = 0
  }

-- | Create an isolated context for a subagent
--
-- Shares: ctxTools, ctxInterrupt, ctxToolContext
-- Isolated: ctxHistory (fresh), ctxIteration (fresh)
-- Copied: ctxConfig (with adjusted maxIterations)
createSubagentContext
  :: AgentContext     -- ^ Parent context
  -> SubagentConfig   -- ^ Subagent configuration
  -> IO AgentContext
createSubagentContext parent cfg = do
  -- Fresh history (empty)
  newHistory <- newTVarIO []

  -- Fresh iteration counter
  newIteration <- newTVarIO 0

  -- Copy config with adjusted max iterations
  parentConfig <- readTVarIO (parent ^. ctxConfig)
  let childConfig = parentConfig & acMaxIterations .~ _sacMaxIterations cfg
  newConfigVar <- newTVarIO childConfig

  pure AgentContext
    { _ctxHistory     = newHistory              -- ISOLATED
    , _ctxTools       = parent ^. ctxTools      -- SHARED
    , _ctxInterrupt   = parent ^. ctxInterrupt  -- SHARED (propagates)
    , _ctxIteration   = newIteration            -- ISOLATED
    , _ctxConfig      = newConfigVar            -- COPIED
    , _ctxToolContext = parent ^. ctxToolContext -- SHARED
    , _ctxPruneState  = parent ^. ctxPruneState  -- SHARED (inherits parent's pruning state)
    }

-- | Run a subagent with the given configuration
-- This is a type-level placeholder that will be filled in by the Loop module
-- to avoid circular dependencies
runSubagent
  :: Members '[Embed IO] r
  => (AgentContext -> Text -> Sem r SubagentResult)  -- ^ The actual runner (injected to avoid circular deps)
  -> AgentContext                                     -- ^ Parent context
  -> SubagentConfig                                   -- ^ Subagent configuration
  -> Sem r SubagentResult
runSubagent runner parentCtx cfg = do
  -- Check depth limit
  if _sacCurrentDepth cfg >= _sacMaxDepth cfg
    then pure $ SubagentError "Maximum subagent depth exceeded"
    else do
      -- Create child context
      childCtx <- embed $ createSubagentContext parentCtx cfg
      -- Run with the provided runner
      runner childCtx (_sacPrompt cfg)
