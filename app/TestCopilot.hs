{-# LANGUAGE OverloadedStrings #-}

module TestCopilot (main) where

import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Network.HTTP.Client      ( Manager )
import           Network.HTTP.Client.TLS  ( newTlsManager )

import           Telos.Core.Types         ( AssistantMessage(..), Message(..) )
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
    Left _      -> do
      putStrLn "No valid saved token, starting OAuth device flow..."
      deviceResult <- initiateDeviceFlow auth

      case deviceResult of
        Left err  -> putStrLn $ "Auth error: " <> show err
        Right dcr -> do
          putStrLn ""
          putStrLn "=========================================="
          putStrLn "Please visit: "
          TIO.putStrLn $ dcrVerificationUri dcr
          putStrLn ""
          putStrLn "And enter code: "
          TIO.putStrLn $ dcrUserCode dcr
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
  let config = CopilotConfig { ccModel = "grok-code-fast-1", ccMaxTokens = Just 1024 }
  let client = newCopilotClient auth mgr config

  -- Test 1: List models
  putStrLn "=== Test 1: List Models ==="
  modelsResult <- listModels client
  case modelsResult of
    Left err     -> TIO.putStrLn $ "Error listing models: " <> err
    Right models -> do
      putStrLn $ "Found " <> show (length (mrData models)) <> " models:"
      for_ (mrData models) $ \model -> do
        TIO.putStrLn $ "  - " <> miId model

  putStrLn ""

  -- Test 2: Send a simple message
  putStrLn "=== Test 2: Send Message (grok-code-fast-1) ==="
  let messages = [ UserMessage "Hello! What is 2 + 2? Reply briefly." ]

  chatResult <- sendChatRequest client messages []
  case chatResult of
    Left err   -> TIO.putStrLn $ "Error: " <> err
    Right resp -> do
      putStrLn "Response received!"
      putStrLn $ "Model: " <> T.unpack (chModel resp)
      case chChoices resp of
        []           -> putStrLn "No choices in response"
        (choice : _) -> case chMessage choice of
          Nothing  -> putStrLn "No message in choice"
          Just msg -> do
            putStrLn "Assistant:"
            TIO.putStrLn $ fromMaybe "(no content)" (amContent msg)

  putStrLn ""
  putStrLn "=== Test Complete ==="
