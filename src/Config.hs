{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config
  , ConfigFile
  , bashAllowFromConfig
  , bashPromptOnDenyFromConfig
  , loadConfigFile
  , saveConfig
  , addBashAllowCommand
  , HasApiKey(..)
  , HasBaseUrl(..)
  , HasModel(..)
  , HasTemperature(..)
  , HasProvider(..)
  , HasProviderSpec(..)
  , configCodec
  , configPath
  , providersPath
  , loadConfig
  ) where

import           Control.Exception ( IOException, catch )
import           Control.Lens      ( (%~), (.~), (^.), makeFieldsNoPrefix, non )

import qualified Data.Text         as Text
import qualified Data.Text.IO      as TIO

import           Provider.Lua      ( ProviderError(..)
                                   , listProviders
                                   , loadProviderSpec
                                   , providersDir
                                   )
import           Provider.Spec     ( ProviderSpec
                                   , apiKeyEnv
                                   , defaultBaseUrl
                                   , defaultModel
                                   , defaultTemperature
                                   )

import           Relude

import           System.Directory  ( createDirectoryIfMissing, doesFileExist, getHomeDirectory )
import           System.FilePath   ( (</>), takeDirectory )

import           Toml              ( (.=), TomlCodec )
import qualified Toml

-- Provider is script-defined (Lua) and stored as free-form text.

data BashToolConfig = BashToolConfig { _bashAllow :: [ Text ], _bashPromptOnDeny :: Bool }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''BashToolConfig

newtype ToolsConfig = ToolsConfig { _bash :: BashToolConfig }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ToolsConfig

data Config
  = Config { _provider     :: Text
           , _apiKey       :: Text
           , _baseUrl      :: Text
           , _model        :: Text
           , _temperature  :: Double
           , _tools        :: ToolsConfig
           , _providerSpec :: ProviderSpec
           }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''Config

data ConfigFile
  = ConfigFile { _fileProvider    :: Text
               , _fileApiKey      :: Text
               , _fileBaseUrl     :: Maybe Text
               , _fileModel       :: Maybe Text
               , _fileTemperature :: Maybe Double
               , _fileTools       :: ToolsConfig
               }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ConfigFile

configCodec :: TomlCodec ConfigFile
configCodec
  = ConfigFile <$> Toml.text "provider" .= _fileProvider
  <*> Toml.text "api_key" .= _fileApiKey
  <*> Toml.dioptional (Toml.text "base_url") .= _fileBaseUrl
  <*> Toml.dioptional (Toml.text "model") .= _fileModel
  <*> Toml.dioptional (Toml.double "temperature") .= _fileTemperature
  <*> toolsCodec .= _fileTools

toolsCodec :: TomlCodec ToolsConfig
toolsCodec
  = Toml.dimatch
    (Just . Just)
    (fromMaybe defaultToolsConfig)
    (Toml.dioptional (Toml.table toolsInnerCodec "tools"))
  where
    toolsInnerCodec :: TomlCodec ToolsConfig
    toolsInnerCodec = ToolsConfig <$> bashTableCodec .= _bash

    bashTableCodec :: TomlCodec BashToolConfig
    bashTableCodec
      = Toml.dimatch
        (Just . Just)
        (fromMaybe defaultBashToolConfig)
        (Toml.dioptional (Toml.table bashCodec "bash"))

    bashCodec :: TomlCodec BashToolConfig
    bashCodec
      = BashToolConfig <$> bashAllowCodec .= _bashAllow <*> bashPromptCodec .= _bashPromptOnDeny

    bashAllowCodec :: TomlCodec [ Text ]
    bashAllowCodec
      = Toml.dimatch
        (Just . Just)
        (fromMaybe [])
        (Toml.dioptional (Toml.arrayOf Toml._Text "allow"))

    bashPromptCodec :: TomlCodec Bool
    bashPromptCodec
      = Toml.dimatch (Just . Just) (fromMaybe True) (Toml.dioptional (Toml.bool "prompt_on_deny"))

data LegacyConfig
  = LegacyConfig { _legacyApiKey      :: Text
                 , _legacyBaseUrl     :: Text
                 , _legacyModel       :: Text
                 , _legacyTemperature :: Double
                 }
  deriving ( Eq, Show, Generic )

legacyConfigCodec :: TomlCodec LegacyConfig
legacyConfigCodec
  = LegacyConfig <$> Toml.text "api_key" .= _legacyApiKey
  <*> Toml.text "base_url" .= _legacyBaseUrl
  <*> Toml.text "model" .= _legacyModel
  <*> Toml.double "temperature" .= _legacyTemperature

defaultConfigFile :: ConfigFile
defaultConfigFile
  = ConfigFile { _fileProvider    = ""
               , _fileApiKey      = ""
               , _fileBaseUrl     = Nothing
               , _fileModel       = Nothing
               , _fileTemperature = Nothing
               , _fileTools       = defaultToolsConfig
               }

defaultBashToolConfig :: BashToolConfig
defaultBashToolConfig = BashToolConfig { _bashAllow = [], _bashPromptOnDeny = True }

defaultToolsConfig :: ToolsConfig
defaultToolsConfig = ToolsConfig { _bash = defaultBashToolConfig }

bashAllowFromConfig :: Config -> [ Text ]
bashAllowFromConfig cfg = cfg ^. tools . bash . bashAllow

bashPromptOnDenyFromConfig :: Config -> Bool
bashPromptOnDenyFromConfig cfg = cfg ^. tools . bash . bashPromptOnDeny

addBashAllowCommand :: Text -> ConfigFile -> ConfigFile
addBashAllowCommand cmd cfg = cfg & fileTools . bash . bashAllow %~ appendUnique cmd
  where
    appendUnique x xs
      | x `elem` xs = xs
      | otherwise = xs <> [ x ]

renderConfigFileToml :: ConfigFile -> Text
renderConfigFileToml = Toml.encode configCodec

configPath :: IO FilePath
configPath = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let base = mXdg ^. non (home </> ".config")
  pure (base </> "telos" </> "config.toml")

providersPath :: IO FilePath
providersPath = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let base = mXdg ^. non (home </> ".config")
  pure (base </> "telos" </> "providers")

-- | Load config from file. If missing, create it with defaults.
-- This function does not apply environment overrides.
loadConfigFile :: IO (Either Text ConfigFile)
loadConfigFile = do
  path <- configPath
  exists <- doesFileExist path
  if not exists
    then do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path (renderConfigFileToml defaultConfigFile)
      pure (Right defaultConfigFile)
    else do
      decoded <- Toml.decodeFileExact configCodec path
      case decoded of
        Right cfg -> pure (Right cfg)
        Left err  -> do
          legacy <- Toml.decodeFileExact legacyConfigCodec path
          case legacy of
            Right legacyCfg -> do
              let cfg
                    = ConfigFile { _fileProvider    = ""
                                 , _fileApiKey      = _legacyApiKey legacyCfg
                                 , _fileBaseUrl     = Just (_legacyBaseUrl legacyCfg)
                                 , _fileModel       = Just (_legacyModel legacyCfg)
                                 , _fileTemperature = Just (_legacyTemperature legacyCfg)
                                 , _fileTools       = defaultToolsConfig
                                 }
              TIO.writeFile path (renderConfigFileToml cfg)
              pure (Right cfg)
            Left _          -> pure $ Left (Text.pack (show err))

saveConfig :: ConfigFile -> IO ()
saveConfig cfg = do
  path <- configPath
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (renderConfigFileToml cfg)

loadConfig :: IO (Either Text Config)
loadConfig = do
  loaded <- loadConfigFile
  case loaded of
    Left err      -> pure (Left err)
    Right fileCfg -> do
      let configured = Text.strip (fileCfg ^. fileProvider)
      providerChoice <- if Text.null configured
        then do
          available <- listProviders
          dir <- providersDir
          pure $ case available of
            []       -> Left
              ("No provider scripts found in "
               <> Text.pack dir
               <> ". Create '<name>.lua' there and set `provider = \"<name>\"` in config.toml.")
            [ only ] -> Right only
            xs       -> Left
              ("Provider is not configured. Set `provider` in config.toml. Available providers: "
               <> Text.intercalate ", " xs)
        else pure (Right configured)

      case providerChoice of
        Left msg           -> pure (Left msg)
        Right providerName -> do
          -- If provider was not configured, persist the auto-selected single provider.
          when (Text.null configured) $ do
            let cfg' = fileCfg & fileProvider .~ providerName
            saveConfig cfg' `catch` \(_ :: IOException) -> pure ()

          specResult <- loadProviderSpec providerName
          case specResult of
            Left specErr -> pure (Left (renderProviderError specErr))
            Right spec   -> do
              let resolved = resolveConfig spec (fileCfg & fileProvider .~ providerName)
              mKey <- lookupEnv (Text.unpack (spec ^. apiKeyEnv))
              let resolved' = maybe resolved (\k -> resolved & apiKey .~ Text.pack k) mKey
              pure (Right resolved')

resolveConfig :: ProviderSpec -> ConfigFile -> Config
resolveConfig spec fileCfg
  = Config { _provider     = fileCfg ^. fileProvider
           , _apiKey       = fileCfg ^. fileApiKey
           , _baseUrl      = fromMaybe (spec ^. defaultBaseUrl) (fileCfg ^. fileBaseUrl)
           , _model        = fromMaybe (spec ^. defaultModel) (fileCfg ^. fileModel)
           , _temperature  = fromMaybe (spec ^. defaultTemperature) (fileCfg ^. fileTemperature)
           , _tools        = fileCfg ^. fileTools
           , _providerSpec = spec
           }

renderProviderError :: ProviderError -> Text
renderProviderError = \case
  ProviderScriptNotFound name path available -> let
      availableText
        = if null available
          then "(none found)"
          else Text.intercalate ", " available
    in 
      "Provider '"
      <> name
      <> "' not found at: "
      <> Text.pack path
      <> "\nAvailable providers: "
      <> availableText
  ProviderLuaNotFound -> "Lua interpreter not found (need lua or luajit in PATH)"
  ProviderLuaFailed msg -> "Provider Lua failed: " <> msg
  ProviderSpecDecodeFailed msg -> "Provider spec decode failed: " <> msg
  ProviderRequestDecodeFailed msg -> "Provider request decode failed: " <> msg
