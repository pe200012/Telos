{-# LANGUAGE OverloadedStrings #-}

module TestCopilot ( main ) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Lens.Micro               ( (^.), non )

import           Network.HTTP.Client      ( Manager )
import           Network.HTTP.Client.TLS  ( newTlsManager )

import           Telos.Core.Types         ( Message(UserMessage), amContent )
import           Telos.LLM.Copilot.Auth
import           Telos.LLM.Copilot.Client

main :: IO ()
main = do
  putStrLn "=== Telos Copilot API Test ==="
  putStrLn ""

  -- Create HTTP manager
  mgr <- newTlsManager

  -- Create auth
  auth <- newCopilotAuth mgr

  -- Try to load saved token first
  putStrLn "Checking for saved token..."
  savedResult <- loadSavedToken auth

  case savedResult of
    Right _tok -> do
      putStrLn "Loaded saved token successfully!"
      runTests auth mgr
    Left _     -> do
      putStrLn "No valid saved token, starting OAuth device flow..."
      deviceResult <- initiateDeviceFlow auth

      case deviceResult of
        Left err  -> putStrLn $ "Auth error: " <> show err
        Right dcr -> do
          putStrLn ""
          putStrLn "=========================================="
          putStrLn "Please visit: "
          TIO.putStrLn $ dcr ^. dcrVerificationUri
          putStrLn ""
          putStrLn "And enter code: "
          TIO.putStrLn $ dcr ^. dcrUserCode
          putStrLn "=========================================="
          putStrLn ""
          putStrLn "Waiting for authorization..."

          -- Poll for token
          tokenResult <- pollForToken auth dcr

          case tokenResult of
            Left err   -> putStrLn $ "Token error: " <> show err
            Right _tok -> do
              putStrLn "Authenticated successfully!"
              putStrLn "Token saved to ~/.config/telos/token.json"
              runTests auth mgr

runTests :: CopilotAuth -> Manager -> IO ()
runTests auth mgr = do
  putStrLn ""

  -- Create client with grok-code-fast-1 model
  let config = CopilotConfig { _ccModel = "grok-code-fast-1", _ccMaxTokens = Just 1024 }
  let client = newCopilotClient auth mgr config

  -- Test 1: List models
  putStrLn "=== Test 1: List Models ==="
  modelsResult <- listModels client
  case modelsResult of
    Left err     -> TIO.putStrLn $ "Error listing models: " <> err
    Right models -> do
      putStrLn $ "Found " <> show (length (models ^. mrData)) <> " models:"
      for_ (models ^. mrData) $ \model -> do
        TIO.putStrLn $ "  - " <> (model ^. miId)

  putStrLn ""

  -- Test 2: Send a simple message
  putStrLn "=== Test 2: Send Message (grok-code-fast-1) ==="
  let messages = [ UserMessage "Hello! What is 2 + 2? Reply briefly." ]

  chatResult <- sendChatRequest client messages []
  case chatResult of
    Left err   -> TIO.putStrLn $ "Error: " <> err
    Right resp -> do
      putStrLn "Response received!"
      putStrLn $ "Model: " <> T.unpack (resp ^. chModel)
      case resp ^. chChoices of
        []           -> putStrLn "No choices in response"
        (choice : _) -> case choice ^. chMessage of
          Nothing  -> putStrLn "No message in choice"
          Just msg -> do
            putStrLn "Assistant:"
            TIO.putStrLn $ msg ^. amContent . non "(no content)"

  putStrLn ""
  putStrLn "=== Test Complete ==="
