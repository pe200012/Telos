{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config
  , HasApiKey(..)
  , HasBaseUrl(..)
  , HasModel(..)
  , HasTemperature(..)
  , configCodec
  , configPath
  , loadConfig
  ) where

import           Control.Lens       ( (&), (.~), (^.), makeFieldsNoPrefix, non )

import           Data.Text          ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO

import           GHC.Generics       ( Generic )

import           System.Directory   ( createDirectoryIfMissing, doesFileExist, getHomeDirectory )
import           System.Environment ( lookupEnv )
import           System.FilePath    ( (</>), takeDirectory )

import           Toml               ( (.=), TomlCodec )
import qualified Toml

data Config = Config { _apiKey :: Text, _baseUrl :: Text, _model :: Text, _temperature :: Double }
  deriving ( Eq, Show, Generic )

makeFieldsNoPrefix ''Config

configCodec :: TomlCodec Config
configCodec
  = Config <$> Toml.text "api_key" .= _apiKey
  <*> Toml.text "base_url" .= _baseUrl
  <*> Toml.text "model" .= _model
  <*> Toml.double "temperature" .= _temperature

defaultConfig :: Config
defaultConfig
  = Config { _apiKey      = ""
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
        Left err  -> pure $ Left (Text.pack (show err))
        Right cfg -> do
          mKey <- lookupEnv "ZHIPUAI_API_KEY"
          let cfg' = maybe cfg (\k -> cfg & apiKey .~ Text.pack k) mKey
          pure (Right cfg')
