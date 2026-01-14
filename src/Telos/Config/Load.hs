
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.Config.Load
  ( loadConfig
  , loadConfigFrom
  , loadGlobalConfig
  , loadProjectConfig
  , globalConfigPath
  , findProjectConfig
  , mergeConfigs
  , applyEnvOverrides
  , ConfigError(..)
  , renderConfigError
  ) where

import           Control.Lens         ( (.~), (?~), (^.), at, non )

import           Data.Aeson           ( eitherDecodeFileStrict', encode )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map

import           Relude

import           System.Directory     ( XdgDirectory(..)
                                      , createDirectoryIfMissing
                                      , doesFileExist
                                      , getCurrentDirectory
                                      , getXdgDirectory
                                      )
import           System.FilePath      ( (</>), takeDirectory )

import           Telos.Config.Types

data ConfigError
  = ConfigNotFound FilePath
  | ConfigParseError FilePath Text
  | ConfigValidationError Text
  deriving stock ( Show, Eq )

renderConfigError :: ConfigError -> Text
renderConfigError = \case
  ConfigNotFound path       -> "Config file not found: " <> toText path
  ConfigParseError path err -> "Failed to parse config at " <> toText path <> ": " <> err
  ConfigValidationError err -> "Config validation error: " <> err

globalConfigPath :: IO FilePath
globalConfigPath = do
  configDir <- getXdgDirectory XdgConfig "telos"
  pure $ configDir </> "telos.json"

findProjectConfig :: IO (Maybe FilePath)
findProjectConfig = do
  cwd <- getCurrentDirectory
  findUp cwd
  where
    configNames = [ "telos.json", ".telos" </> "config.json" ]

    findUp :: FilePath -> IO (Maybe FilePath)
    findUp dir = findFirst (map (dir </>) configNames) >>= \case
      Just path -> pure $ Just path
      Nothing   -> do
        let parent = takeDirectory dir
        if parent == dir
          then pure Nothing
          else findUp parent

    findFirst :: [ FilePath ] -> IO (Maybe FilePath)
    findFirst []       = pure Nothing
    findFirst (p : ps) = do
      exists <- doesFileExist p
      if exists
        then pure $ Just p
        else findFirst ps

loadConfigFrom :: FilePath -> IO (Either ConfigError TelosConfig)
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ ConfigNotFound path
    else eitherDecodeFileStrict' path >>= \case
      Right cfg -> pure $ Right cfg
      Left err  -> pure $ Left $ ConfigParseError path (toText err)

loadGlobalConfig :: IO TelosConfig
loadGlobalConfig = do
  path <- globalConfigPath
  result <- loadConfigFrom path
  case result of
    Right cfg -> pure cfg
    Left (ConfigNotFound _) -> do
      createDefaultConfig path
      pure defaultTelosConfig
    Left err -> do
      putTextLn $ "Warning: " <> renderConfigError err
      pure defaultTelosConfig

createDefaultConfig :: FilePath -> IO ()
createDefaultConfig path = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  LBS.writeFile path (encode defaultTelosConfig)
  putTextLn $ "Created default config at: " <> toText path

loadProjectConfig :: IO (Maybe TelosConfig)
loadProjectConfig = findProjectConfig >>= \case
  Nothing   -> pure Nothing
  Just path -> do
    loadConfigFrom path >>= \case
      Right cfg -> pure $ Just cfg
      Left err  -> do
        putTextLn $ "Warning: " <> renderConfigError err
        pure Nothing

mergeConfigs :: TelosConfig -> TelosConfig -> TelosConfig
mergeConfigs base override
  = TelosConfig
  { _tcModel = override ^. tcModel
  , _tcSmallModel = override ^. tcSmallModel <|> base ^. tcSmallModel
  , _tcProviders = Map.unionWith mergeProvider (base ^. tcProviders) (override ^. tcProviders)
  , _tcMcp = Map.union (override ^. tcMcp) (base ^. tcMcp)
  , _tcPermissions = Map.union (override ^. tcPermissions) (base ^. tcPermissions)
  , _tcInstructions = base ^. tcInstructions <> override ^. tcInstructions
  , _tcCompaction = mergeCompaction (base ^. tcCompaction) (override ^. tcCompaction)
  , _tcMaxIterations = override ^. tcMaxIterations
  , _tcStreamingEnabled = override ^. tcStreamingEnabled
  , _tcSnapshotEnabled = override ^. tcSnapshotEnabled
  , _tcLogLevel = override ^. tcLogLevel
  }
  where
    mergeProvider :: ProviderConfig -> ProviderConfig -> ProviderConfig
    mergeProvider b o
      = ProviderConfig { _pcApiKey  = o ^. pcApiKey <|> b ^. pcApiKey
                       , _pcBaseURL = o ^. pcBaseURL <|> b ^. pcBaseURL
                       , _pcTimeout = o ^. pcTimeout <|> b ^. pcTimeout
                       }

    mergeCompaction :: CompactionConfig -> CompactionConfig -> CompactionConfig
    mergeCompaction _ o = o

applyEnvOverrides :: TelosConfig -> IO TelosConfig
applyEnvOverrides cfg = do
  mModel <- lookupEnv "TELOS_MODEL"
  mLogLevel <- lookupEnv "TELOS_LOG_LEVEL"
  mOpenAIKey <- lookupEnv "OPENAI_API_KEY"
  mAnthropicKey <- lookupEnv "ANTHROPIC_API_KEY"
  mGoogleKey <- lookupEnv "GOOGLE_API_KEY"
  mMistralKey <- lookupEnv "MISTRAL_API_KEY"
  mMaxIter <- lookupEnv "TELOS_MAX_ITERATIONS"

  pure
    $ cfg
    & maybe id ((tcModel .~) . toText) mModel
    & maybe id (tcLogLevel .~) (mLogLevel >>= logLevelFromText . toText)
    & maybe id (setProviderKey "openai" . toText) mOpenAIKey
    & maybe id (setProviderKey "anthropic" . toText) mAnthropicKey
    & maybe id (setProviderKey "google" . toText) mGoogleKey
    & maybe id (setProviderKey "mistral" . toText) mMistralKey
    & maybe id (tcMaxIterations .~) (mMaxIter >>= readMaybe)
  where
    -- Set the API key for a given provider in the config
    -- If the provider does not exist, it is created with default config
    setProviderKey :: Text -> Text -> TelosConfig -> TelosConfig
    setProviderKey name key = tcProviders . at name . non defaultProviderConfig . pcApiKey ?~ key

loadConfig :: IO TelosConfig
loadConfig = do
  global <- loadGlobalConfig
  mProject <- loadProjectConfig
  let merged = case mProject of
        Nothing      -> global
        Just project -> mergeConfigs global project
  applyEnvOverrides merged
