module Telos.Core.Error ( LLMError(..), MCPError(..), ProcessError(..), AppError(..) ) where

import           Data.Text    ( Text )

import           GHC.Generics ( Generic )

data LLMError
  = LLMAuthError Text
  | LLMRateLimited Int
  | LLMInvalidRequest Text
  | LLMNetworkError Text
  | LLMParseError Text
  | LLMTimeout
  | LLMUnknownError Text
  deriving stock ( Eq, Show, Generic )

data MCPError
  = MCPConnectionFailed Text
  | MCPInitializeFailed Text
  | MCPToolNotFound Text
  | MCPToolExecutionFailed Text
  | MCPResourceNotFound Text
  | MCPProtocolError Int Text
  | MCPTimeout
  | MCPServerCrashed Text
  deriving stock ( Eq, Show, Generic )

data ProcessError = ProcessSpawnFailed Text | ProcessNotRunning | ProcessIOError Text
  deriving stock ( Eq, Show, Generic )

data AppError
  = AppLLMError LLMError
  | AppMCPError MCPError
  | AppProcessError ProcessError
  | AppConfigError Text
  | AppInterrupted
  deriving stock ( Eq, Show, Generic )
