-- | AGENTS.md discovery module
-- Recursively searches for AGENTS.md files from current directory upward,
-- including global config directory.
module Telos.Prompt.Discovery
  ( discoverAgentsRules
  , findAgentsFiles
  , getGlobalAgentsFile
  ) where

import qualified Data.Text.IO         as TIO

import           System.Directory     ( XdgDirectory (XdgConfig)
                                      , doesFileExist
                                      , getXdgDirectory
                                      )
import           System.FilePath      ( takeDirectory, (</>) )

import           Telos.Prompt.Types   ( PromptSection, makePromptSection )

-- | Discover all AGENTS.md files from cwd upward + global config
-- Returns sections with increasing priority (global first, most specific last)
discoverAgentsRules :: FilePath -> IO [ PromptSection ]
discoverAgentsRules startDir = do
  -- 1. Check global config first (lowest priority)
  globalFile <- getGlobalAgentsFile

  -- 2. Find project AGENTS.md files (from root down to cwd)
  projectFiles <- findAgentsFiles startDir

  -- 3. Combine: global first, then project files (root to cwd)
  let allFiles = maybe [] pure globalFile <> projectFiles

  -- 4. Read and create sections with increasing priority
  zipWithM readAgentsFile [100, 110 ..] allFiles

-- | Read an AGENTS.md file and create a PromptSection
readAgentsFile :: Int -> FilePath -> IO PromptSection
readAgentsFile priority path = do
  content <- TIO.readFile path
  let sectionName = "rules:" <> toText path
      wrappedContent = "Instructions from: " <> toText path <> "\n" <> content
  pure $ makePromptSection sectionName wrappedContent priority

-- | Recursively find AGENTS.md files from dir upward to root
-- Returns files in order from root to the starting directory (parent first)
findAgentsFiles :: FilePath -> IO [ FilePath ]
findAgentsFiles dir = go dir []
  where
    go currentDir acc = do
      let agentsPath = currentDir </> "AGENTS.md"
      exists <- doesFileExist agentsPath
      let current = [ agentsPath | exists ]
          parent  = takeDirectory currentDir

      if parent == currentDir
        -- Reached filesystem root, return accumulated (will be in root-to-leaf order)
        then pure $ current <> acc
        -- Continue upward, prepending current to accumulator
        else go parent (current <> acc)

-- | Get global AGENTS.md path from XDG config directory
-- Returns Nothing if file doesn't exist
getGlobalAgentsFile :: IO (Maybe FilePath)
getGlobalAgentsFile = do
  configDir <- getXdgDirectory XdgConfig "telos"
  let path = configDir </> "AGENTS.md"
  exists <- doesFileExist path
  pure $ if exists then Just path else Nothing
