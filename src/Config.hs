{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config
  , Provider(..)
  , HasApiKey(..)
  , HasBaseUrl(..)
  , HasModel(..)
  , HasTemperature(..)
  , HasProvider(..)
  , configCodec
  , configPath
  , loadConfig
  ) where

import           Control.Lens     ( (.~), (^.), makeFieldsNoPrefix, non )

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

data Config
  = Config { _provider    :: Provider
           , _apiKey      :: Text
           , _baseUrl     :: Text
           , _model       :: Text
           , _temperature :: Double
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
           }

renderConfigToml :: Config -> Text
renderConfigToml = Toml.encode configCodec

configPath :: IO FilePath
configPath = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let base = mXdg ^. non (home </> ".config")
  pure (base </> "telos" </> "config.toml")

loadConfig :: IO (Either Text Config)
loadConfig = do
  path <- configPath
  exists <- doesFileExist path
  if not exists
    then do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path (renderConfigToml defaultConfig)
      pure $ Left ("created default config: " <> Text.pack path)
    else do
      decoded <- Toml.decodeFileExact configCodec path
      case decoded of
        Right cfg -> do
          mKey <- lookupEnv (apiKeyEnv (cfg ^. provider))
          let cfg' = maybe cfg (\k -> cfg & apiKey .~ Text.pack k) mKey
          pure (Right cfg')
        Left err  -> do
          legacy <- Toml.decodeFileExact legacyConfigCodec path
          case legacy of
            Right cfg -> do
              TIO.writeFile path (renderConfigToml cfg)
              mKey <- lookupEnv (apiKeyEnv (cfg ^. provider))
              let cfg' = maybe cfg (\k -> cfg & apiKey .~ Text.pack k) mKey
              pure (Right cfg')
            Left _    -> pure $ Left (Text.pack (show err))

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
