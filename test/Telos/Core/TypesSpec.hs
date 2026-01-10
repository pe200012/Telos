module Telos.Core.TypesSpec (spec) where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson (decode, encode, eitherDecode)
import Data.Aeson.Types (Value (..))
import qualified Data.HashMap.Strict as HM
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Telos.Core.Types

spec :: Spec
spec = do
  describe "Role" $ do
    it "serializes User to 'user'" $
      encode User `shouldBe` "\"user\""

    it "serializes Assistant to 'assistant'" $
      encode Assistant `shouldBe` "\"assistant\""

    it "serializes System to 'system'" $
      encode System `shouldBe` "\"system\""

    it "serializes ToolRole to 'tool'" $
      encode ToolRole `shouldBe` "\"tool\""

    it "roundtrips all roles" $ do
      decode (encode User) `shouldBe` Just User
      decode (encode Assistant) `shouldBe` Just Assistant
      decode (encode System) `shouldBe` Just System
      decode (encode ToolRole) `shouldBe` Just ToolRole

    it "fails on unknown role" $
      (eitherDecode "\"unknown\"" :: Either String Role) `shouldSatisfy` isLeft

  describe "ToolCall" $ do
    let sampleToolCall = ToolCall
          { tcId = "call_123"
          , tcName = "get_weather"
          , tcArguments = Object $ HM.fromList [("city", String "Tokyo")]
          }

    it "serializes to expected JSON structure" $ do
      let json = encode sampleToolCall
      decode json `shouldBe` Just sampleToolCall

    it "roundtrips ToolCall" $ property $ \(tcId', tcName') ->
      let tc = ToolCall tcId' tcName' (Object HM.empty)
      in decode (encode tc) == Just tc

  describe "Tool" $ do
    let sampleTool = Tool
          { toolName = "read_file"
          , toolDescription = Just "Read a file from disk"
          , toolInputSchema = Object $ HM.fromList
              [ ("type", String "object")
              , ("properties", Object $ HM.fromList
                  [("path", Object $ HM.fromList [("type", String "string")])])
              ]
          }

    it "roundtrips Tool" $
      decode (encode sampleTool) `shouldBe` Just sampleTool

    it "handles missing description" $ do
      let toolNoDesc = Tool "test" Nothing (Object HM.empty)
      decode (encode toolNoDesc) `shouldBe` Just toolNoDesc

  describe "Message" $ do
    describe "UserMessage" $ do
      it "serializes with role 'user'" $ do
        let msg = UserMessage "Hello"
            json = encode msg
        json `shouldSatisfy` \j -> "\"role\":\"user\"" `BS.isInfixOf` j

      it "roundtrips" $ property $ \content ->
        let msg = UserMessage content
        in decode (encode msg) == Just msg

    describe "SystemMessage" $ do
      it "serializes with role 'system'" $ do
        let msg = SystemMessage "You are helpful"
            json = encode msg
        json `shouldSatisfy` \j -> "\"role\":\"system\"" `BS.isInfixOf` j

      it "roundtrips" $ property $ \content ->
        let msg = SystemMessage content
        in decode (encode msg) == Just msg

    describe "AssistantMessage" $ do
      it "handles content only" $ do
        let msg = AssistantMessage (Just "Hello!") []
        decode (encode msg) `shouldBe` Just msg

      it "handles tool calls only" $ do
        let tc = ToolCall "id1" "func" (Object HM.empty)
            msg = AssistantMessage Nothing [tc]
        decode (encode msg) `shouldBe` Just msg

      it "handles both content and tool calls" $ do
        let tc = ToolCall "id1" "func" (Object HM.empty)
            msg = AssistantMessage (Just "Let me help") [tc]
        decode (encode msg) `shouldBe` Just msg

    describe "ToolResultMessage" $ do
      it "serializes with role 'tool'" $ do
        let msg = ToolResultMessage "call_1" "get_weather" "Sunny" False
            json = encode msg
        json `shouldSatisfy` \j -> "\"role\":\"tool\"" `BS.isInfixOf` j

      it "includes tool_call_id" $ do
        let msg = ToolResultMessage "call_abc" "test" "result" False
            json = encode msg
        json `shouldSatisfy` \j -> "\"tool_call_id\":\"call_abc\"" `BS.isInfixOf` j

      it "roundtrips" $ do
        let msg = ToolResultMessage "id" "name" "result" True
        decode (encode msg) `shouldBe` Just msg

  describe "StreamEvent" $ do
    it "ContentDelta roundtrips" $ property $ \txt ->
      let event = ContentDelta txt
      in decode (encode event) == Just event

    it "ToolCallStart roundtrips" $ do
      let event = ToolCallStart 0 "call_1" "func_name"
      decode (encode event) `shouldBe` Just event

    it "ToolCallDelta roundtrips" $ do
      let event = ToolCallDelta 0 "{\"arg\":"
      decode (encode event) `shouldBe` Just event

    it "Ping roundtrips" $
      decode (encode Ping) `shouldBe` Just Ping

  describe "StreamResult" $ do
    it "StreamCompleted roundtrips" $ do
      let msg = AssistantMessage (Just "Done") []
          result = StreamCompleted msg
      decode (encode result) `shouldBe` Just result

    it "StreamInterrupted roundtrips" $ do
      let partial = PartialMessage "partial content" []
          result = StreamInterrupted partial
      decode (encode result) `shouldBe` Just result

    it "StreamFailed roundtrips" $ do
      let result = StreamFailed "connection error"
      decode (encode result) `shouldBe` Just result

  describe "ProviderInfo" $ do
    it "roundtrips" $ do
      let info = ProviderInfo "Copilot" "gpt-4" True (Just 16384)
      decode (encode info) `shouldBe` Just info

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
