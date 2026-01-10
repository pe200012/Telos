{-# LANGUAGE TemplateHaskell #-}

module Telos.Agent.Context
  ( AgentContext
  , ctxHistory
  , ctxTools
  , ctxInterrupt
  , ctxIteration
  , ctxConfig
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
  ) where

import           Lens.Micro.TH       ( makeLenses )

import           Telos.Agent.Config  ( AgentConfig )
import           Telos.Core.Types    ( Message, Tool )

data AgentContext = AgentContext
  { _ctxHistory   :: TVar [ Message ]
  , _ctxTools     :: TVar [ Tool ]
  , _ctxInterrupt :: MVar ()
  , _ctxIteration :: TVar Int
  , _ctxConfig    :: AgentConfig
  }

makeLenses ''AgentContext

newAgentContext :: AgentConfig -> IO AgentContext
newAgentContext cfg = do
  history <- newTVarIO []
  tools <- newTVarIO []
  interrupt <- newEmptyMVar
  iteration <- newTVarIO 0
  pure AgentContext
    { _ctxHistory   = history
    , _ctxTools     = tools
    , _ctxInterrupt = interrupt
    , _ctxIteration = iteration
    , _ctxConfig    = cfg
    }

addMessage :: AgentContext -> Message -> IO ()
addMessage ctx msg = atomically $ modifyTVar' (_ctxHistory ctx) (++ [ msg ])

getHistory :: AgentContext -> IO [ Message ]
getHistory ctx = readTVarIO (_ctxHistory ctx)

setHistory :: AgentContext -> [ Message ] -> IO ()
setHistory ctx msgs = atomically $ writeTVar (_ctxHistory ctx) msgs

clearHistory :: AgentContext -> IO ()
clearHistory ctx = atomically $ writeTVar (_ctxHistory ctx) []

getTools :: AgentContext -> IO [ Tool ]
getTools ctx = readTVarIO (_ctxTools ctx)

registerTools :: AgentContext -> [ Tool ] -> IO ()
registerTools ctx tools = atomically $ writeTVar (_ctxTools ctx) tools

getIterationCount :: AgentContext -> IO Int
getIterationCount ctx = readTVarIO (_ctxIteration ctx)

incrementIteration :: AgentContext -> IO Int
incrementIteration ctx = atomically $ do
  modifyTVar' (_ctxIteration ctx) (+ 1)
  readTVar (_ctxIteration ctx)

resetIteration :: AgentContext -> IO ()
resetIteration ctx = atomically $ writeTVar (_ctxIteration ctx) 0
