module Telos.MCP.Transport.StdIOSpec (spec) where

import Data.Aeson (object, (.=))
import Test.Hspec

import Telos.MCP.Transport.StdIO

spec :: Spec
spec = do
  describe "spawnMCPProcess" $ do
    it "spawns a simple process successfully" $ do
      result <- spawnMCPProcess "cat" [] Nothing Nothing
      case result of
        Left err -> expectationFailure $ "Failed to spawn process: " <> show err
        Right handle -> do
          closeHandle handle

    it "returns error for non-existent command" $ do
      result <- spawnMCPProcess "/nonexistent/command/that/does/not/exist" [] Nothing Nothing
      case result of
        Left _ -> pure ()
        Right handle -> do
          closeHandle handle
          expectationFailure "Should have failed for non-existent command"

  describe "sendMessage and receiveMessage" $ do
    it "sends and receives JSON messages through cat" $ do
      result <- spawnMCPProcess "cat" [] Nothing Nothing
      case result of
        Left err -> expectationFailure $ "Failed to spawn: " <> show err
        Right handle -> do
          let testMsg = object ["test" .= ("hello" :: String), "number" .= (42 :: Int)]
          sendResult <- sendMessage handle testMsg
          case sendResult of
            Left err -> do
              closeHandle handle
              expectationFailure $ "Failed to send: " <> show err
            Right () -> do
              recvResult <- receiveMessage handle
              closeHandle handle
              case recvResult of
                Left err -> expectationFailure $ "Failed to receive: " <> show err
                Right received -> received `shouldBe` testMsg

  describe "closeHandle" $ do
    it "closes handle without error" $ do
      result <- spawnMCPProcess "cat" [] Nothing Nothing
      case result of
        Left err -> expectationFailure $ "Failed to spawn: " <> show err
        Right handle -> do
          closeHandle handle
