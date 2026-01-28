{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module CLI.Context
  ( ContextSpec
  , defaultContextSpec
  , HasPaths(..)
  , HasMaxBytes(..)
  , HasMaxPerFile(..)
  , addContextPath
  , clearContextSpec
  , buildContextMessage
  , validatePathSpec
  ) where

import           Control.Lens       ( (%~), (&), (.~), (^.) )
import           Control.Lens.TH    ( makeFieldsNoPrefix )

import           Data.List          ( isPrefixOf, nub )
import           Data.Text          ( Text )
import qualified Data.Text          as Text

import           Effects.FileSystem ( FileSystem, listFiles, readText )

import           Polysemy           ( Embed, Members, Sem, embed )

import           System.Directory   ( canonicalizePath )
import           System.FilePath    ( (</>)
                                    , addTrailingPathSeparator
                                    , isAbsolute
                                    , makeRelative
                                    , normalise
                                    )

import           Types.Chat         ( Message, Role(System), mkMessage )

data ContextSpec = ContextSpec { _paths :: [ FilePath ], _maxBytes :: Int, _maxPerFile :: Int }

makeFieldsNoPrefix ''ContextSpec

defaultContextSpec :: ContextSpec
defaultContextSpec = ContextSpec { _paths = [], _maxBytes = 120000, _maxPerFile = 8000 }

addContextPath :: FilePath -> ContextSpec -> ContextSpec
addContextPath path = paths %~ (<> [ path ])

clearContextSpec :: ContextSpec -> ContextSpec
clearContextSpec spec = spec & paths .~ []

validatePathSpec :: FilePath -> Text -> IO (Either Text FilePath)
validatePathSpec scopeRoot raw = do
  let trimmed = Text.strip raw
  if Text.null trimmed
    then pure (Left "Empty path.")
    else do
      let textPath = Text.unpack trimmed
      if hasWildcard trimmed
        then validateWildcard scopeRoot textPath
        else validateConcrete scopeRoot textPath

buildContextMessage
  :: Members '[ FileSystem, Embed IO ] r => FilePath -> ContextSpec -> Sem r (Maybe Message)
buildContextMessage scopeRoot spec = do
  files <- resolvePaths scopeRoot (spec ^. paths)
  let unique = nub files
  blocks <- buildBlocks scopeRoot unique (spec ^. maxBytes) (spec ^. maxPerFile)
  if null blocks
    then pure Nothing
    else pure (Just (mkMessage System (wrapBlock (Text.intercalate "\n" blocks))))

resolvePaths
  :: Members '[ FileSystem, Embed IO ] r => FilePath -> [ FilePath ] -> Sem r [ FilePath ]
resolvePaths scopeRoot specs = concat <$> mapM (resolvePathSpec scopeRoot) specs

resolvePathSpec
  :: Members '[ FileSystem, Embed IO ] r => FilePath -> FilePath -> Sem r [ FilePath ]
resolvePathSpec scopeRoot spec = do
  let specText = Text.pack spec
  if hasWildcard specText
    then do
      allFiles <- listFiles scopeRoot
      let matches = filter (matchesGlob specText . normalizeRel scopeRoot) allFiles
      pure matches
    else listFiles (toAbsolute scopeRoot spec)

buildBlocks :: Members '[ FileSystem, Embed IO ] r
            => FilePath
            -> [ FilePath ]
            -> Int
            -> Int
            -> Sem r [ Text ]
buildBlocks scopeRoot files maxBytes maxPerFile = go files 0 []
  where
    go [] _ acc = pure (reverse acc)
    go (path : rest) total acc
      | total >= maxBytes = pure (reverse acc)
      | otherwise = do
        content <- readText path
        case content of
          Nothing   -> go rest total acc
          Just text -> do
            let truncated = truncateText maxPerFile text
                block     = renderBlock (normalizeRel scopeRoot path) truncated
                newTotal  = total + Text.length block
            if newTotal > maxBytes
              then pure (reverse acc)
              else go rest newTotal (block : acc)

renderBlock :: Text -> Text -> Text
renderBlock path content = Text.unlines [ "# path: " <> path, "```", content, "```" ]

truncateText :: Int -> Text -> Text
truncateText maxPerFile text
  = if Text.length text <= maxPerFile
    then text
    else Text.take maxPerFile text <> "\n... [truncated]"

wrapBlock :: Text -> Text
wrapBlock inner = Text.unlines [ "<filesystem>", inner, "</filesystem>" ]

hasWildcard :: Text -> Bool
hasWildcard = Text.any (\c -> c == '*' || c == '?')

normalizeRel :: FilePath -> FilePath -> Text
normalizeRel scopeRoot path = Text.pack (normalise (makeRelative scopeRoot path))

toAbsolute :: FilePath -> FilePath -> FilePath
toAbsolute scopeRoot path
  = if isAbsolute path
    then path
    else scopeRoot </> path

matchesGlob :: Text -> Text -> Bool
matchesGlob patternText pathText = match (Text.unpack patternText) (Text.unpack pathText)

match :: String -> String -> Bool
match [] [] = True
match [] _ = False
match ('*' : ps) str = match ps str || case str of
  []       -> False
  (_ : ss) -> match ('*' : ps) ss
match ('?' : ps) str = case str of
  []       -> False
  (_ : ss) -> match ps ss
match (p : ps) (s : ss)
  | p == s = match ps ss
  | otherwise = False
match _ _ = False

validateConcrete :: FilePath -> FilePath -> IO (Either Text FilePath)
validateConcrete scopeRoot path = do
  scope <- canonicalizePath scopeRoot
  absPath <- canonicalizePath (toAbsolute scopeRoot path)
  if withinScope scope absPath
    then pure (Right path)
    else pure (Left "Path is outside scope.")

validateWildcard :: FilePath -> FilePath -> IO (Either Text FilePath)
validateWildcard scopeRoot path = do
  if isAbsolute path
    then do
      scope <- canonicalizePath scopeRoot
      absPath <- canonicalizePath path
      if withinScope scope absPath
        then pure (Right path)
        else pure (Left "Path is outside scope.")
    else pure (Right path)

withinScope :: FilePath -> FilePath -> Bool
withinScope scopeRoot target
  = let
      scopeNorm  = addTrailingPathSeparator (normalise scopeRoot)
      targetNorm = normalise target
    in 
      scopeNorm `isPrefixOf` targetNorm || targetNorm == normalise scopeRoot
