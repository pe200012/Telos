{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Context
  ( AgentContext(..)
  , ctxHistory
  , ctxTools
  , ctxInterrupt
  , ctxIteration
  , ctxConfig
  , ctxToolContext
  , ctxPruneState
  , newAgentContext
  , addMessage
  , getHistory
  , setHistory
  , getTools
  , registerTools
  , clearHistory
  , getIterationCount
  , incrementIteration
  , resetIteration
  , getPruneState
  , setPruneState
  , modifyPruneState
  ) where

import           Control.Lens         ( (^.) )
import           Control.Lens      ( makeLenses )

import           Relude

import           Telos.Agent.Config ( AgentConfig )
import           Telos.Context.Types ( PruneState, emptyPruneState )
import           Telos.Core.Types   ( Message, Tool )
import           Telos.Tool.Types   ( ToolContext, newToolContext )

data AgentContext
  = AgentContext
  { _ctxHistory     :: TVar [ Message ]
  , _ctxTools       :: TVar [ Tool ]
  , _ctxInterrupt   :: MVar ()
  , _ctxIteration   :: TVar Int
  , _ctxConfig      :: TVar AgentConfig
  , _ctxToolContext :: ToolContext
  , _ctxPruneState  :: TVar PruneState
  }

makeLenses ''AgentContext

newAgentContext :: AgentConfig -> IO AgentContext
newAgentContext cfg = do
  history <- newTVarIO []
  tools <- newTVarIO []
  interrupt <- newEmptyMVar
  iteration <- newTVarIO 0
  configVar <- newTVarIO cfg
  pruneState <- newTVarIO emptyPruneState
  toolCtx <- newToolContext
  pure
    AgentContext { _ctxHistory     = history
                 , _ctxTools       = tools
                 , _ctxInterrupt   = interrupt
                 , _ctxIteration   = iteration
                 , _ctxConfig      = configVar
                 , _ctxToolContext = toolCtx
                 , _ctxPruneState  = pruneState
                 }

addMessage :: AgentContext -> Message -> IO ()
addMessage ctx msg = atomically $ modifyTVar' (ctx ^. ctxHistory) (++ [ msg ])

getHistory :: AgentContext -> IO [ Message ]
getHistory ctx = readTVarIO (ctx ^. ctxHistory)

setHistory :: AgentContext -> [ Message ] -> IO ()
setHistory ctx msgs = atomically $ writeTVar (ctx ^. ctxHistory) msgs

clearHistory :: AgentContext -> IO ()
clearHistory ctx = atomically $ writeTVar (ctx ^. ctxHistory) []

getTools :: AgentContext -> IO [ Tool ]
getTools ctx = readTVarIO (ctx ^. ctxTools)

registerTools :: AgentContext -> [ Tool ] -> IO ()
registerTools ctx tools = atomically $ writeTVar (ctx ^. ctxTools) tools

getIterationCount :: AgentContext -> IO Int
getIterationCount ctx = readTVarIO (ctx ^. ctxIteration)

incrementIteration :: AgentContext -> IO Int
incrementIteration ctx = atomically $ do
  modifyTVar' (ctx ^. ctxIteration) (+ 1)
  readTVar (ctx ^. ctxIteration)

resetIteration :: AgentContext -> IO ()
resetIteration ctx = atomically $ writeTVar (ctx ^. ctxIteration) 0

getPruneState :: AgentContext -> IO PruneState
getPruneState ctx = readTVarIO (ctx ^. ctxPruneState)

setPruneState :: AgentContext -> PruneState -> IO ()
setPruneState ctx ps = atomically $ writeTVar (ctx ^. ctxPruneState) ps

modifyPruneState :: AgentContext -> (PruneState -> PruneState) -> IO ()
modifyPruneState ctx f = atomically $ modifyTVar' (ctx ^. ctxPruneState) f
