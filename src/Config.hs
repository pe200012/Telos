{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config
  , Provider(..)
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
  , configCodec
  , configPath
  , loadConfig
  ) where

import           Control.Lens     ( (%~), (.~), (^.), makeFieldsNoPrefix, non )

import qualified Data.Text        as Text
import qualified Data.Text.IO     as TIO

import           Relude

import           System.Directory ( createDirectoryIfMissing, doesFileExist, getHomeDirectory )
import           System.FilePath  ( (</>), takeDirectory )

import           Toml             ( (.=), TomlCodec )
import qualified Toml

data Provider = ZhipuAI | OpenAI
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''Provider

data BashToolConfig = BashToolConfig { _bashAllow :: [ Text ], _bashPromptOnDeny :: Bool }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''BashToolConfig

newtype ToolsConfig = ToolsConfig { _bash :: BashToolConfig }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''ToolsConfig

data Config
  = Config { _provider    :: Provider
           , _apiKey      :: Text
           , _baseUrl     :: Text
           , _model       :: Text
           , _temperature :: Double
           , _tools       :: ToolsConfig
           }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''Config

configCodec :: TomlCodec Config
configCodec
  = Config <$> providerCodec .= _provider
  <*> Toml.text "api_key" .= _apiKey
  <*> Toml.text "base_url" .= _baseUrl
  <*> Toml.text "model" .= _model
  <*> Toml.double "temperature" .= _temperature
  <*> toolsCodec .= _tools

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

legacyConfigCodec :: TomlCodec Config
legacyConfigCodec
  = (\apiKeyText baseUrlText modelText temp -> defaultConfig
     & apiKey .~ apiKeyText
     & baseUrl .~ baseUrlText
     & model .~ modelText
     & temperature .~ temp) <$> Toml.text "api_key" .= _apiKey
  <*> Toml.text "base_url" .= _baseUrl
  <*> Toml.text "model" .= _model
  <*> Toml.double "temperature" .= _temperature

defaultConfig :: Config
defaultConfig
  = Config { _provider    = ZhipuAI
           , _apiKey      = ""
           , _baseUrl     = "https://open.bigmodel.cn/api/paas/v4/chat/completions"
           , _model       = "glm-4.7-flash"
           , _temperature = 0.7
           , _tools       = defaultToolsConfig
           }

defaultBashToolConfig :: BashToolConfig
defaultBashToolConfig = BashToolConfig { _bashAllow = [], _bashPromptOnDeny = True }

defaultToolsConfig :: ToolsConfig
defaultToolsConfig = ToolsConfig { _bash = defaultBashToolConfig }

bashAllowFromConfig :: Config -> [ Text ]
bashAllowFromConfig cfg = cfg ^. tools . bash . bashAllow

bashPromptOnDenyFromConfig :: Config -> Bool
bashPromptOnDenyFromConfig cfg = cfg ^. tools . bash . bashPromptOnDeny

addBashAllowCommand :: Text -> Config -> Config
addBashAllowCommand cmd cfg = cfg & tools . bash . bashAllow %~ appendUnique cmd
  where
    appendUnique x xs
      | x `elem` xs = xs
      | otherwise = xs <> [ x ]

renderConfigToml :: Config -> Text
renderConfigToml = Toml.encode configCodec

configPath :: IO FilePath
configPath = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let base = mXdg ^. non (home </> ".config")
  pure (base </> "telos" </> "config.toml")

-- | Load config from file. If missing, create it with defaults.
-- This function does not apply environment overrides.
loadConfigFile :: IO (Either Text Config)
loadConfigFile = do
  path <- configPath
  exists <- doesFileExist path
  if not exists
    then do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path (renderConfigToml defaultConfig)
      pure (Right defaultConfig)
    else do
      decoded <- Toml.decodeFileExact configCodec path
      case decoded of
        Right cfg -> pure (Right cfg)
        Left err  -> do
          legacy <- Toml.decodeFileExact legacyConfigCodec path
          case legacy of
            Right cfg -> do
              TIO.writeFile path (renderConfigToml cfg)
              pure (Right cfg)
            Left _    -> pure $ Left (Text.pack (show err))

saveConfig :: Config -> IO ()
saveConfig cfg = do
  path <- configPath
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (renderConfigToml cfg)

loadConfig :: IO (Either Text Config)
loadConfig = do
  loaded <- loadConfigFile
  case loaded of
    Left err  -> pure (Left err)
    Right cfg -> do
      mKey <- lookupEnv (apiKeyEnv (cfg ^. provider))
      let cfg' = maybe cfg (\k -> cfg & apiKey .~ Text.pack k) mKey
      pure (Right cfg')

providerCodec :: TomlCodec Provider
providerCodec = Toml.textBy renderProvider parseProvider "provider"

parseProvider :: Text -> Either Text Provider
parseProvider text = case Text.toLower text of
  "zhipuai" -> Right ZhipuAI
  "openai"  -> Right OpenAI
  _         -> Left "unknown provider, expected zhipuai or openai"

renderProvider :: Provider -> Text
renderProvider ZhipuAI = "zhipuai"
renderProvider OpenAI  = "openai"

apiKeyEnv :: Provider -> String
apiKeyEnv ZhipuAI = "ZHIPUAI_API_KEY"
apiKeyEnv OpenAI  = "OPENAI_API_KEY"
