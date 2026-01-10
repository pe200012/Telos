{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telos.LLM.Copilot.Auth
  ( CopilotAuth(..)
  , TokenState(..)
  , DeviceCodeResponse(..)
  , CopilotToken(..)
  , AuthError(..)
  , newCopilotAuth
  , initiateDeviceFlow
  , pollForToken
  , ensureValidToken
  , getToken
  , loadSavedToken
  ) where

import           Control.Concurrent        ( threadDelay )
import           Control.Concurrent.STM
import           Control.Exception         ( SomeException, try )
import           Control.Monad             ( when )

import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import           Data.Text                 ( Text )
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time                 ( UTCTime, addUTCTime, diffUTCTime, getCurrentTime )

import           GHC.Generics              ( Generic )

import           Network.HTTP.Client
import           Network.HTTP.Types.Status ( statusCode )

import           System.Directory          ( XdgDirectory(..)
                                           , createDirectoryIfMissing
                                           , doesFileExist
                                           , getXdgDirectory
                                           )
import           System.FilePath           ( (</>) )

-- | GitHub OAuth Client ID for Copilot
copilotClientId :: Text
copilotClientId = "Iv1.b507a08c87ecfe98"

-- | Token file path
tokenFilePath :: IO FilePath
tokenFilePath = do
  configDir <- getXdgDirectory XdgConfig "telos"
  pure $ configDir </> "token.json"

-- | Copilot authentication state manager
data CopilotAuth
  = CopilotAuth { caManager    :: Manager
                , caTokenState :: TVar TokenState
                , caOAuthToken :: TVar (Maybe Text)  -- Stored OAuth token for refresh
                }

-- | Token state
data TokenState = NoToken | Authenticating DeviceCodeResponse | Authenticated CopilotToken
  deriving stock ( Show )

-- | Persisted token data
data PersistedToken
  = PersistedToken { ptOAuthToken   :: Text      -- GitHub OAuth access token
                   , ptCopilotToken :: Text      -- Copilot API token
                   , ptExpiresAt    :: UTCTime   -- Copilot token expiry
                   }
  deriving stock ( Show, Generic )

instance ToJSON PersistedToken where
  toJSON pt
    = object
      [ "oauth_token" .= ptOAuthToken pt
      , "copilot_token" .= ptCopilotToken pt
      , "expires_at" .= ptExpiresAt pt
      ]

instance FromJSON PersistedToken where
  parseJSON = withObject "PersistedToken" $ \o
    -> PersistedToken <$> o .: "oauth_token" <*> o .: "copilot_token" <*> o .: "expires_at"

-- | Device code response from GitHub
data DeviceCodeResponse
  = DeviceCodeResponse
  { dcrDeviceCode      :: Text
  , dcrUserCode        :: Text
  , dcrVerificationUri :: Text
  , dcrExpiresIn       :: Int
  , dcrInterval        :: Int
  }
  deriving stock ( Show, Generic )

instance FromJSON DeviceCodeResponse where
  parseJSON = withObject "DeviceCodeResponse" $ \o -> DeviceCodeResponse <$> o .: "device_code"
    <*> o .: "user_code"
    <*> o .: "verification_uri"
    <*> o .: "expires_in"
    <*> o .: "interval"

-- | GitHub OAuth access token response
data OAuthTokenResponse
  = OAuthTokenResponse { otrAccessToken :: Text, otrTokenType :: Text, otrScope :: Text }
  deriving stock ( Show, Generic )

instance FromJSON OAuthTokenResponse where
  parseJSON = withObject "OAuthTokenResponse" $ \o
    -> OAuthTokenResponse <$> o .: "access_token" <*> o .: "token_type" <*> o .: "scope"

-- | OAuth error response
data OAuthError = OAuthError { oeError :: Text, oeDescription :: Maybe Text }
  deriving stock ( Show, Generic )

instance FromJSON OAuthError where
  parseJSON
    = withObject "OAuthError" $ \o -> OAuthError <$> o .: "error" <*> o .:? "error_description"

-- | Copilot API token (from copilot_internal endpoint)
data CopilotToken = CopilotToken { ctToken :: Text, ctExpiresAt :: UTCTime }
  deriving stock ( Show )

data CopilotTokenResponse
  = CopilotTokenResponse { ctrToken     :: Text
                         , ctrExpiresAt :: Int  -- Unix timestamp
                         }
  deriving stock ( Show, Generic )

instance FromJSON CopilotTokenResponse where
  parseJSON = withObject "CopilotTokenResponse" $ \o
    -> CopilotTokenResponse <$> o .: "token" <*> o .: "expires_at"

-- | Authentication errors
data AuthError
  = AuthNetworkError Text
  | AuthParseError Text
  | AuthExpired
  | AuthDenied Text
  | AuthPending
  | AuthSlowDown
  deriving stock ( Show )

-- | Create new Copilot auth manager
newCopilotAuth :: Manager -> IO CopilotAuth
newCopilotAuth mgr = do
  tokenState <- newTVarIO NoToken
  oauthToken <- newTVarIO Nothing
  pure CopilotAuth { caManager = mgr, caTokenState = tokenState, caOAuthToken = oauthToken }

-- | Load saved token from disk
loadSavedToken :: CopilotAuth -> IO (Either AuthError CopilotToken)
loadSavedToken auth = do
  path <- tokenFilePath
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ AuthDenied "No saved token found"
    else do
      result <- try $ BL.readFile path
      case result of
        Left (e :: SomeException)
          -> pure $ Left $ AuthParseError $ "Failed to read token file: " <> T.pack (show e)
        Right content -> case eitherDecode content of
          Left err -> pure $ Left $ AuthParseError $ T.pack err
          Right pt -> do
            now <- getCurrentTime
            -- Check if Copilot token is still valid (with 5 min buffer)
            if diffUTCTime (ptExpiresAt pt) now > 300
              then do
                -- Token still valid, restore state
                let token = CopilotToken (ptCopilotToken pt) (ptExpiresAt pt)
                atomically $ do
                  writeTVar (caTokenState auth) (Authenticated token)
                  writeTVar (caOAuthToken auth) (Just (ptOAuthToken pt))
                pure $ Right token
              else do
                -- Copilot token expired, try to refresh using OAuth token
                atomically $ writeTVar (caOAuthToken auth) (Just (ptOAuthToken pt))
                refreshResult <- getCopilotToken auth (ptOAuthToken pt)
                case refreshResult of
                  Left err    -> pure $ Left err
                  Right token -> do
                    atomically $ writeTVar (caTokenState auth) (Authenticated token)
                    -- Save refreshed token
                    saveToken auth (ptOAuthToken pt) token
                    pure $ Right token

-- | Save token to disk
saveToken :: CopilotAuth -> Text -> CopilotToken -> IO ()
saveToken _auth oauthTok copilotTok = do
  path <- tokenFilePath
  configDir <- getXdgDirectory XdgConfig "telos"
  createDirectoryIfMissing True configDir
  let persisted
        = PersistedToken { ptOAuthToken   = oauthTok
                         , ptCopilotToken = ctToken copilotTok
                         , ptExpiresAt    = ctExpiresAt copilotTok
                         }
  BL.writeFile path (encode persisted)

-- | Initiate OAuth device flow
initiateDeviceFlow :: CopilotAuth -> IO (Either AuthError DeviceCodeResponse)
initiateDeviceFlow auth = do
  let reqBody = "client_id=" <> TE.encodeUtf8 copilotClientId <> "&scope=read:user"

  initReq <- parseRequest "https://github.com/login/device/code"
  let req
        = initReq { method         = "POST"
                  , requestBody    = RequestBodyBS reqBody
                  , requestHeaders = [ ( "Accept", "application/json" )
                                     , ( "Content-Type", "application/x-www-form-urlencoded" )
                                     ]
                  }

  resp <- httpLbs req (caManager auth)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) of
      Left err  -> pure $ Left $ AuthParseError $ T.pack err
      Right dcr -> do
        atomically $ writeTVar (caTokenState auth) (Authenticating dcr)
        pure $ Right dcr
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> T.pack (show code)

-- | Poll for token after user authorizes
pollForToken :: CopilotAuth -> DeviceCodeResponse -> IO (Either AuthError CopilotToken)
pollForToken auth dcr = pollLoop (dcrInterval dcr)
  where
    pollLoop interval = do
      threadDelay (interval * 1000000)
      result <- requestOAuthToken auth (dcrDeviceCode dcr)
      case result of
        Left AuthPending  -> pollLoop interval
        Left AuthSlowDown -> pollLoop (interval + 5)
        Left err          -> pure $ Left err
        Right oauthToken  -> do
          -- Store OAuth token for future refresh
          atomically $ writeTVar (caOAuthToken auth) (Just (otrAccessToken oauthToken))
          -- Exchange OAuth token for Copilot API token
          copilotResult <- getCopilotToken auth (otrAccessToken oauthToken)
          case copilotResult of
            Left err    -> pure $ Left err
            Right token -> do
              atomically $ writeTVar (caTokenState auth) (Authenticated token)
              -- Save to disk
              saveToken auth (otrAccessToken oauthToken) token
              pure $ Right token

-- | Request OAuth access token
requestOAuthToken :: CopilotAuth -> Text -> IO (Either AuthError OAuthTokenResponse)
requestOAuthToken auth deviceCode = do
  let reqBody
        = mconcat
          [ "client_id="
          , TE.encodeUtf8 copilotClientId
          , "&device_code="
          , TE.encodeUtf8 deviceCode
          , "&grant_type=urn:ietf:params:oauth:grant-type:device_code"
          ]

  initReq <- parseRequest "https://github.com/login/oauth/access_token"
  let req
        = initReq { method         = "POST"
                  , requestBody    = RequestBodyBS reqBody
                  , requestHeaders = [ ( "Accept", "application/json" )
                                     , ( "Content-Type", "application/x-www-form-urlencoded" )
                                     ]
                  }

  resp <- httpLbs req (caManager auth)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) :: Either String OAuthError of
      Right oauthErr -> case oeError oauthErr of
        "authorization_pending" -> pure $ Left AuthPending
        "slow_down" -> pure $ Left AuthSlowDown
        "expired_token" -> pure $ Left AuthExpired
        "access_denied" -> pure $ Left $ AuthDenied "User denied access"
        err -> pure $ Left $ AuthDenied err
      Left _         -> case eitherDecode (responseBody resp) of
        Left err    -> pure $ Left $ AuthParseError $ T.pack err
        Right token -> pure $ Right token
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> T.pack (show code)

-- | Get Copilot API token from OAuth token
getCopilotToken :: CopilotAuth -> Text -> IO (Either AuthError CopilotToken)
getCopilotToken auth oauthToken = do
  initReq <- parseRequest "https://api.github.com/copilot_internal/v2/token"
  let req
        = initReq { requestHeaders = [ ( "Authorization", "token " <> TE.encodeUtf8 oauthToken )
                                     , ( "Accept", "application/json" )
                                     , ( "User-Agent", "Telos/0.1.0" )
                                     ]
                  }

  resp <- httpLbs req (caManager auth)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) of
      Left err  -> pure $ Left $ AuthParseError $ T.pack err
      Right ctr -> do
        now <- getCurrentTime
        let expiresAt
              = addUTCTime
                (fromIntegral (ctrExpiresAt ctr) - realToFrac (utcTimeToPOSIXSeconds now))
                now
        pure $ Right CopilotToken { ctToken = ctrToken ctr, ctExpiresAt = expiresAt }
    401  -> pure $ Left $ AuthDenied "Invalid OAuth token"
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> T.pack (show code)
  where
    utcTimeToPOSIXSeconds :: UTCTime -> Double
    utcTimeToPOSIXSeconds t = realToFrac $ diffUTCTime t (read "1970-01-01 00:00:00 UTC")

-- | Ensure we have a valid token, refreshing if needed
ensureValidToken :: CopilotAuth -> IO (Either AuthError CopilotToken)
ensureValidToken auth = do
  state <- atomically $ readTVar (caTokenState auth)
  case state of
    NoToken -> do
      -- Try loading from disk first
      loadResult <- loadSavedToken auth
      case loadResult of
        Right token -> pure $ Right token
        Left _      -> pure $ Left $ AuthDenied "Not authenticated. Call initiateDeviceFlow first."
    Authenticating _ -> pure $ Left AuthPending
    Authenticated token -> do
      now <- getCurrentTime
      if diffUTCTime (ctExpiresAt token) now < 300  -- Refresh if < 5 min left
        then do
          -- Try to refresh using stored OAuth token
          mOAuth <- atomically $ readTVar (caOAuthToken auth)
          case mOAuth of
            Nothing       -> pure $ Left AuthExpired
            Just oauthTok -> do
              refreshResult <- getCopilotToken auth oauthTok
              case refreshResult of
                Left err       -> pure $ Left err
                Right newToken -> do
                  atomically $ writeTVar (caTokenState auth) (Authenticated newToken)
                  saveToken auth oauthTok newToken
                  pure $ Right newToken
        else pure $ Right token

-- | Get current token if available
getToken :: CopilotAuth -> IO (Maybe CopilotToken)
getToken auth = do
  state <- atomically $ readTVar (caTokenState auth)
  case state of
    Authenticated token -> pure $ Just token
    _ -> pure Nothing
