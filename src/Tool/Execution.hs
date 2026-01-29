{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Tool.Execution ( executeToolCall ) where

import           Control.Exception  ( IOException, catch )
import           Control.Lens       ( (^.), non )

import           Data.Aeson         ( (.:)
                                    , FromJSON(parseJSON)
                                    , Value
                                    , eitherDecodeStrict'
                                    , withObject
                                    )
import           Data.Aeson.Types   ( parseEither )
import qualified Data.Text          as Text
import           Data.Text.Encoding ( encodeUtf8 )

import           Effects.FileSystem ( FileSystem, listFiles, readText, writeText )

import           Polysemy           ( Embed, Members, Sem, embed )

import           Relude             hiding ( encodeUtf8 )

import           System.FilePath    ( makeRelative )
import           System.Process     ( readProcess )

import           Tool.Registry      ( ToolResult, findTool, mkToolResult )

import           Types.ToolCall     ( ToolCall
                                    , functionArguments
                                    , functionName
                                    , toolCallId
                                    , toolFunction
                                    )

newtype ListFilesArgs = ListFilesArgs { listPath :: FilePath }
  deriving ( Eq, Show, Generic )

newtype ReadFileArgs = ReadFileArgs { readPath :: FilePath }
  deriving ( Eq, Show, Generic )

data WriteFileArgs = WriteFileArgs { writePath :: FilePath, writeContent :: Text }
  deriving ( Eq, Show, Generic )

data GrepArgs = GrepArgs { grepPath :: FilePath, grepPattern :: Text }
  deriving ( Eq, Show, Generic )

data BashArgs = BashArgs { bashCommand :: Text, bashArgs :: Maybe [ Text ] }
  deriving ( Eq, Show, Generic )

newtype ApplyPatchArgs = ApplyPatchArgs { patchText :: Text }
  deriving ( Eq, Show, Generic )

instance FromJSON ListFilesArgs where
  parseJSON = withObject "ListFilesArgs" $ \obj -> ListFilesArgs <$> obj .: "path"

instance FromJSON ReadFileArgs where
  parseJSON = withObject "ReadFileArgs" $ \obj -> ReadFileArgs <$> obj .: "path"

instance FromJSON WriteFileArgs where
  parseJSON
    = withObject "WriteFileArgs" $ \obj -> WriteFileArgs <$> obj .: "path" <*> obj .: "content"

instance FromJSON GrepArgs where
  parseJSON = withObject "GrepArgs" $ \obj -> GrepArgs <$> obj .: "path" <*> obj .: "pattern"

instance FromJSON BashArgs where
  parseJSON = withObject "BashArgs" $ \obj -> BashArgs <$> obj .: "command" <*> obj .: "args"

instance FromJSON ApplyPatchArgs where
  parseJSON = withObject "ApplyPatchArgs" $ \obj -> ApplyPatchArgs <$> obj .: "patch"

executeToolCall :: Members '[ FileSystem, Embed IO ] r => FilePath -> ToolCall -> Sem r ToolResult
executeToolCall scopeRoot call = do
  let name   = call ^. toolFunction . functionName
      callId = call ^. toolCallId
  case findTool name of
    Nothing -> pure (mkToolResult callId name "Unknown tool")
    Just _  -> do
      let argsText = call ^. toolFunction . functionArguments
      case eitherDecodeStrict' (encodeUtf8 argsText) of
        Left err    -> pure
          (mkToolResult callId name ("Invalid arguments JSON: " <> Text.pack err))
        Right value -> runTool scopeRoot name callId value

runTool
  :: Members '[ FileSystem, Embed IO ] r => FilePath -> Text -> Text -> Value -> Sem r ToolResult
runTool scopeRoot name callId value
  | name == "list_files" = do
    parseResult <- parseArgs @ListFilesArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        files <- listFiles (listPath args)
        let rendered = Text.unlines (map (Text.pack . makeRelative scopeRoot) files)
        pure (mkToolResult callId name rendered)
  | name == "read_file" = do
    parseResult <- parseArgs @ReadFileArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        content <- readText (readPath args)
        let rendered = content ^. non ""
        pure (mkToolResult callId name rendered)
  | name == "write_file" = do
    parseResult <- parseArgs @WriteFileArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        writeText (writePath args) (writeContent args)
        pure (mkToolResult callId name "ok")
  | name == "grep" = do
    parseResult <- parseArgs @GrepArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        files <- listFiles (grepPath args)
        matches <- fmap catMaybes $ forM files $ \file -> do
          text <- readText file
          pure
            $ if maybe False (Text.isInfixOf (grepPattern args)) text
              then Just (Text.pack (makeRelative scopeRoot file))
              else Nothing
        pure (mkToolResult callId name (Text.unlines matches))
  | name == "bash" = do
    parseResult <- parseArgs @BashArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        result <- embed @IO $ runBash args
        pure (mkToolResult callId name result)
  | name == "apply_patch" = do
    parseResult <- parseArgs @ApplyPatchArgs value
    case parseResult of
      Left err   -> pure (mkToolResult callId name err)
      Right args -> do
        result <- embed @IO $ runApplyPatch scopeRoot (patchText args)
        case result of
          Left err  -> pure (mkToolResult callId name err)
          Right out -> if Text.null out
            then pure (mkToolResult callId name "ok")
            else pure (mkToolResult callId name out)
  | otherwise = pure (mkToolResult callId name "Unknown tool")

parseArgs :: FromJSON a => Value -> Sem r (Either Text a)
parseArgs value = case parseEither parseJSON value of
  Left err  -> pure (Left (Text.pack err))
  Right val -> pure (Right val)

runBash :: BashArgs -> IO Text
runBash bashSpec = do
  let cmd = bashCommand bashSpec
  if cmd `elem` allowedCommands
    then do
      let argv = maybe [] (map Text.unpack) (bashArgs bashSpec)
      (Text.pack <$> readProcess (Text.unpack cmd) argv "") `catch` \(e :: IOException) -> pure
        (Text.pack (displayException e))
    else pure "Command not allowed."

allowedCommands :: [ Text ]
allowedCommands = [ "git", "stack", "ls" ]

runApplyPatch :: FilePath -> Text -> IO (Either Text Text)
runApplyPatch scopeRoot patch
  = (Right . Text.pack
     <$> readProcess "git" [ "-C", scopeRoot, "apply", "--whitespace=nowarn" ] (Text.unpack patch))
  `catch` \(e :: IOException) -> pure (Left (Text.pack (displayException e)))
