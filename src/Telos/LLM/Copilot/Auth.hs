{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telos.LLM.Copilot.Auth
  ( CopilotAuth(..)
  , caManager
  , caTokenState
  , caOAuthToken
  , TokenState(..)
  , DeviceCodeResponse(..)
  , dcrDeviceCode
  , dcrUserCode
  , dcrVerificationUri
  , dcrExpiresIn
  , dcrInterval
  , CopilotToken(..)
  , ctToken
  , ctExpiresAt
  , AuthError(..)
  , newCopilotAuth
  , initiateDeviceFlow
  , pollForToken
  , ensureValidToken
  , getToken
  , loadSavedToken
    -- Export unused lenses to suppress warnings
  , otrTokenType
  , otrScope
  , oeDescription
  ) where

import           Control.Concurrent        ( threadDelay )
import           Control.Exception         ( try )

import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text.Encoding        as TE
import           Data.Time                 ( UTCTime, addUTCTime, diffUTCTime, getCurrentTime )
import           Data.Time.Clock.POSIX     ( utcTimeToPOSIXSeconds )

import           Lens.Micro                ( (^.) )
import           Lens.Micro.TH             ( makeLenses )

import           Network.HTTP.Client
import           Network.HTTP.Types.Status ( statusCode )

import           Relude

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

-- | Copilot API token with expiry
data CopilotToken = CopilotToken { _ctToken :: Text, _ctExpiresAt :: UTCTime }
  deriving stock ( Show, Generic )

makeLenses ''CopilotToken

-- | Device code response from GitHub
data DeviceCodeResponse
  = DeviceCodeResponse
  { _dcrDeviceCode      :: Text
  , _dcrUserCode        :: Text
  , _dcrVerificationUri :: Text
  , _dcrExpiresIn       :: Int
  , _dcrInterval        :: Int
  }
  deriving stock ( Show, Generic )

makeLenses ''DeviceCodeResponse

-- | Token state
data TokenState = NoToken | Authenticating DeviceCodeResponse | Authenticated CopilotToken
  deriving stock ( Show )

-- | Copilot authentication state manager
data CopilotAuth
  = CopilotAuth { _caManager    :: Manager
                , _caTokenState :: TVar TokenState
                , _caOAuthToken :: TVar (Maybe Text)  -- Stored OAuth token for refresh
                }

makeLenses ''CopilotAuth

-- | Persisted token data
data PersistedToken
  = PersistedToken { _ptOAuthToken   :: Text      -- GitHub OAuth access token
                   , _ptCopilotToken :: Text      -- Copilot API token
                   , _ptExpiresAt    :: UTCTime   -- Copilot token expiry
                   }
  deriving stock ( Show, Generic )

makeLenses ''PersistedToken

instance ToJSON PersistedToken where
  toJSON pt
    = object
      [ "oauth_token" .= (pt ^. ptOAuthToken)
      , "copilot_token" .= (pt ^. ptCopilotToken)
      , "expires_at" .= (pt ^. ptExpiresAt)
      ]

instance FromJSON PersistedToken where
  parseJSON = withObject "PersistedToken" $ \o
    -> PersistedToken <$> o .: "oauth_token" <*> o .: "copilot_token" <*> o .: "expires_at"

instance FromJSON DeviceCodeResponse where
  parseJSON = withObject "DeviceCodeResponse" $ \o -> DeviceCodeResponse <$> o .: "device_code"
    <*> o .: "user_code"
    <*> o .: "verification_uri"
    <*> o .: "expires_in"
    <*> o .: "interval"

-- | GitHub OAuth access token response
data OAuthTokenResponse
  = OAuthTokenResponse { _otrAccessToken :: Text, _otrTokenType :: Text, _otrScope :: Text }
  deriving stock ( Show, Generic )

makeLenses ''OAuthTokenResponse

instance FromJSON OAuthTokenResponse where
  parseJSON = withObject "OAuthTokenResponse" $ \o
    -> OAuthTokenResponse <$> o .: "access_token" <*> o .: "token_type" <*> o .: "scope"

-- | OAuth error response
data OAuthError = OAuthError { _oeError :: Text, _oeDescription :: Maybe Text }
  deriving stock ( Show, Generic )

makeLenses ''OAuthError

instance FromJSON OAuthError where
  parseJSON
    = withObject "OAuthError" $ \o -> OAuthError <$> o .: "error" <*> o .:? "error_description"

data CopilotTokenResponse
  = CopilotTokenResponse { _ctrToken     :: Text
                         , _ctrExpiresAt :: Int  -- Unix timestamp
                         }
  deriving stock ( Show, Generic )

makeLenses ''CopilotTokenResponse

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
  pure CopilotAuth { _caManager = mgr, _caTokenState = tokenState, _caOAuthToken = oauthToken }

-- | Load saved token from disk
loadSavedToken :: CopilotAuth -> IO (Either AuthError CopilotToken)
loadSavedToken auth = do
  tokenPath <- tokenFilePath
  exists <- doesFileExist tokenPath
  if not exists
    then pure $ Left $ AuthDenied "No saved token found"
    else do
      result <- try $ BL.readFile tokenPath
      case result of
        Left (e :: SomeException)
          -> pure $ Left $ AuthParseError $ "Failed to read token file: " <> show e
        Right content -> case eitherDecode content of
          Left err -> pure $ Left $ AuthParseError $ toText err
          Right pt -> do
            now <- getCurrentTime
            -- Check if Copilot token is still valid (with 5 min buffer)
            if diffUTCTime (pt ^. ptExpiresAt) now > 300
              then do
                -- Token still valid, restore state
                let token = CopilotToken (pt ^. ptCopilotToken) (pt ^. ptExpiresAt)
                atomically $ do
                  writeTVar (auth ^. caTokenState) (Authenticated token)
                  writeTVar (auth ^. caOAuthToken) (Just (pt ^. ptOAuthToken))
                pure $ Right token
              else do
                -- Copilot token expired, try to refresh using OAuth token
                atomically $ writeTVar (auth ^. caOAuthToken) (Just (pt ^. ptOAuthToken))
                refreshResult <- getCopilotToken auth (pt ^. ptOAuthToken)
                case refreshResult of
                  Left err    -> pure $ Left err
                  Right token -> do
                    atomically $ writeTVar (auth ^. caTokenState) (Authenticated token)
                    -- Save refreshed token
                    saveToken auth (pt ^. ptOAuthToken) token
                    pure $ Right token

-- | Save token to disk
saveToken :: CopilotAuth -> Text -> CopilotToken -> IO ()
saveToken _auth oauthTok copilotTok = do
  tokenPath <- tokenFilePath
  configDir <- getXdgDirectory XdgConfig "telos"
  createDirectoryIfMissing True configDir
  let persisted
        = PersistedToken { _ptOAuthToken   = oauthTok
                         , _ptCopilotToken = copilotTok ^. ctToken
                         , _ptExpiresAt    = copilotTok ^. ctExpiresAt
                         }
  BL.writeFile tokenPath (encode persisted)

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

  resp <- httpLbs req (auth ^. caManager)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) of
      Left err  -> pure $ Left $ AuthParseError $ toText err
      Right dcr -> do
        atomically $ writeTVar (auth ^. caTokenState) (Authenticating dcr)
        pure $ Right dcr
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> show code

-- | Poll for token after user authorizes
pollForToken :: CopilotAuth -> DeviceCodeResponse -> IO (Either AuthError CopilotToken)
pollForToken auth dcr = pollLoop (dcr ^. dcrInterval)
  where
    pollLoop interval = do
      threadDelay (interval * 1000000)
      result <- requestOAuthToken auth (dcr ^. dcrDeviceCode)
      case result of
        Left AuthPending  -> pollLoop interval
        Left AuthSlowDown -> pollLoop (interval + 5)
        Left err          -> pure $ Left err
        Right oauthToken  -> do
          -- Store OAuth token for future refresh
          atomically $ writeTVar (auth ^. caOAuthToken) (Just (oauthToken ^. otrAccessToken))
          -- Exchange OAuth token for Copilot API token
          copilotResult <- getCopilotToken auth (oauthToken ^. otrAccessToken)
          case copilotResult of
            Left err    -> pure $ Left err
            Right token -> do
              atomically $ writeTVar (auth ^. caTokenState) (Authenticated token)
              -- Save to disk
              saveToken auth (oauthToken ^. otrAccessToken) token
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

  resp <- httpLbs req (auth ^. caManager)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) :: Either String OAuthError of
      Right oauthErr -> case oauthErr ^. oeError of
        "authorization_pending" -> pure $ Left AuthPending
        "slow_down" -> pure $ Left AuthSlowDown
        "expired_token" -> pure $ Left AuthExpired
        "access_denied" -> pure $ Left $ AuthDenied "User denied access"
        err -> pure $ Left $ AuthDenied err
      Left _         -> case eitherDecode (responseBody resp) of
        Left err    -> pure $ Left $ AuthParseError $ toText err
        Right token -> pure $ Right token
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> show code

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

  resp <- httpLbs req (auth ^. caManager)

  case statusCode (responseStatus resp) of
    200  -> case eitherDecode (responseBody resp) of
      Left err  -> pure $ Left $ AuthParseError $ toText err
      Right ctr -> do
        now <- getCurrentTime
        let expiresAt
              = addUTCTime (fromIntegral (ctr ^. ctrExpiresAt) - utcTimeToPOSIXSeconds now) now
        pure $ Right CopilotToken { _ctToken = ctr ^. ctrToken, _ctExpiresAt = expiresAt }
    401  -> pure $ Left $ AuthDenied "Invalid OAuth token"
    code -> pure $ Left $ AuthNetworkError $ "HTTP " <> show code

-- | Ensure we have a valid token, refreshing if needed
ensureValidToken :: CopilotAuth -> IO (Either AuthError CopilotToken)
ensureValidToken auth = do
  tokenState <- readTVarIO (auth ^. caTokenState)
  case tokenState of
    NoToken -> do
      -- Try loading from disk first
      loadResult <- loadSavedToken auth
      case loadResult of
        Right token -> pure $ Right token
        Left _      -> pure $ Left $ AuthDenied "Not authenticated. Call initiateDeviceFlow first."
    Authenticating _ -> pure $ Left AuthPending
    Authenticated token -> do
      now <- getCurrentTime
      if diffUTCTime (token ^. ctExpiresAt) now < 300  -- Refresh if < 5 min left
        then do
          -- Try to refresh using stored OAuth token
          mOAuth <- readTVarIO (auth ^. caOAuthToken)
          case mOAuth of
            Nothing       -> pure $ Left AuthExpired
            Just oauthTok -> do
              refreshResult <- getCopilotToken auth oauthTok
              case refreshResult of
                Left err       -> pure $ Left err
                Right newToken -> do
                  atomically $ writeTVar (auth ^. caTokenState) (Authenticated newToken)
                  saveToken auth oauthTok newToken
                  pure $ Right newToken
        else pure $ Right token

-- | Get current token if available
getToken :: CopilotAuth -> IO (Maybe CopilotToken)
getToken auth = do
  tokenState <- readTVarIO (auth ^. caTokenState)
  case tokenState of
    Authenticated token -> pure $ Just token
    _ -> pure Nothing
