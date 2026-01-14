{-# LANGUAGE TemplateHaskell #-}

module Telos.Prompt.Types
  ( PromptSection
  , makePromptSection
  , psName
  , psContent
  , psPriority
  , SystemPromptConfig
  , makeSystemPromptConfig
  , simpleSystemPromptConfig
  , spcWorkingDir
  , spcIsGitRepo
  , spcPlatform
  , spcTools
  , spcModelId
  , spcCurrentDate
  ) where

import           Control.Lens     ( makeLenses )

import           Relude

import           Telos.Core.Types ( Tool )

-- | A section of the system prompt
data PromptSection
  = PromptSection { _psName     :: Text   -- ^ Section identifier (e.g., "identity", "tools")
                  , _psContent  :: Text   -- ^ The actual prompt content
                  , _psPriority :: Int    -- ^ Lower = earlier in final prompt
                  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''PromptSection

-- | Smart constructor for PromptSection
makePromptSection :: Text -> Text -> Int -> PromptSection
makePromptSection name content priority
  = PromptSection { _psName = name, _psContent = content, _psPriority = priority }

-- | Configuration for building system prompts
data SystemPromptConfig
  = SystemPromptConfig
  { _spcWorkingDir  :: FilePath   -- ^ Current working directory
  , _spcIsGitRepo   :: Bool       -- ^ Whether cwd is a git repository
  , _spcPlatform    :: Text       -- ^ Platform (linux, darwin, windows)
  , _spcTools       :: [ Tool ]   -- ^ Available tools
  , _spcModelId     :: Text       -- ^ Model identifier for future routing
  , _spcCurrentDate :: Text       -- ^ Current date string
  }
  deriving stock ( Eq, Show, Generic )

makeLenses ''SystemPromptConfig

-- | Smart constructor for SystemPromptConfig
makeSystemPromptConfig
  :: FilePath -> Bool -> Text -> [ Tool ] -> Text -> Text -> SystemPromptConfig
makeSystemPromptConfig workDir isGit platform tools modelId date
  = SystemPromptConfig
  { _spcWorkingDir  = workDir
  , _spcIsGitRepo   = isGit
  , _spcPlatform    = platform
  , _spcTools       = tools
  , _spcModelId     = modelId
  , _spcCurrentDate = date
  }

-- | Create a simple system prompt config with just a static prompt
-- For testing or simple use cases where environment info is not needed
simpleSystemPromptConfig :: Text -> SystemPromptConfig
simpleSystemPromptConfig _prompt
  = SystemPromptConfig
  { _spcWorkingDir  = "."
  , _spcIsGitRepo   = False
  , _spcPlatform    = "unknown"
  , _spcTools       = []
  , _spcModelId     = "default"
  , _spcCurrentDate = ""
  }
