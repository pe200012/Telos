module Telos.Agent.Context
  ( AgentContext(..)
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

import           Telos.Agent.Config      ( AgentConfig )
import           Telos.Core.Types        ( Message, Tool )

-- | Agent execution context with mutable state
data AgentContext
  = AgentContext { ctxHistory   :: TVar [ Message ]   -- ^ Conversation history (newest last)
                 , ctxTools     :: TVar [ Tool ]      -- ^ Available tools from MCP servers
                 , ctxInterrupt :: MVar ()          -- ^ Interrupt signal (Ctrl+C)
                 , ctxIteration :: TVar Int         -- ^ Current iteration count
                 , ctxConfig    :: AgentConfig      -- ^ Agent configuration
                 }

-- | Create a new agent context
newAgentContext :: AgentConfig -> IO AgentContext
newAgentContext cfg = do
  history <- newTVarIO []
  tools <- newTVarIO []
  interrupt <- newEmptyMVar
  iteration <- newTVarIO 0
  pure
    AgentContext { ctxHistory   = history
                 , ctxTools     = tools
                 , ctxInterrupt = interrupt
                 , ctxIteration = iteration
                 , ctxConfig    = cfg
                 }

-- | Add a message to the conversation history
addMessage :: AgentContext -> Message -> IO ()
addMessage ctx msg = atomically $ modifyTVar' (ctxHistory ctx) (++ [ msg ])

-- | Get the current conversation history
getHistory :: AgentContext -> IO [ Message ]
getHistory ctx = readTVarIO (ctxHistory ctx)

-- | Set the conversation history (replace entirely)
setHistory :: AgentContext -> [ Message ] -> IO ()
setHistory ctx msgs = atomically $ writeTVar (ctxHistory ctx) msgs

-- | Clear the conversation history
clearHistory :: AgentContext -> IO ()
clearHistory ctx = atomically $ writeTVar (ctxHistory ctx) []

-- | Get available tools
getTools :: AgentContext -> IO [ Tool ]
getTools ctx = readTVarIO (ctxTools ctx)

-- | Register tools (replaces existing)
registerTools :: AgentContext -> [ Tool ] -> IO ()
registerTools ctx tools = atomically $ writeTVar (ctxTools ctx) tools

-- | Get current iteration count
getIterationCount :: AgentContext -> IO Int
getIterationCount ctx = readTVarIO (ctxIteration ctx)

-- | Increment iteration count, returns new value
incrementIteration :: AgentContext -> IO Int
incrementIteration ctx = atomically $ do
  modifyTVar' (ctxIteration ctx) (+ 1)
  readTVar (ctxIteration ctx)

-- | Reset iteration count to 0
resetIteration :: AgentContext -> IO ()
resetIteration ctx = atomically $ writeTVar (ctxIteration ctx) 0
