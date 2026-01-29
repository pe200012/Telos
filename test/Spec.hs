module Main ( main ) where

import           CLI.Context             ( buildContextMessage
                                         , defaultContextSpec
                                         , maxBytes
                                         , maxPerFile
                                         , paths
                                         , validatePathSpec
                                         )

import           Control.Exception       ( bracket )
import           Control.Lens            ( (.~), view )

import           Crypto.Hash             ( Digest, SHA256, hash )

import           Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8   as BS8
import qualified Data.Text               as Text
import           Data.Text.Encoding      ( encodeUtf8 )

import           FileSystem.Local        ( runFileSystemLocal )

import           Markdown.RenderAnsi     ( renderMarkdown )
import           Markdown.Stream         ( finalizeStream, newStreamState, pushDelta )

import           Polysemy                ( runM )

import           Relude                  hiding ( encodeUtf8, lookupEnv )

import           Snapshot.Git            ( createProject
                                         , listProjects
                                         , projectName
                                         , renameProject
                                         )

import           System.Directory        ( createDirectoryIfMissing
                                         , getTemporaryDirectory
                                         , removeDirectoryRecursive
                                         )
import           System.Environment      ( lookupEnv, setEnv, unsetEnv )
import           System.FilePath         ( (</>), takeDirectory )
import           System.IO.Temp          ( createTempDirectory )

import           Test.Hspec              ( Spec
                                         , anyException
                                         , describe
                                         , hspec
                                         , it
                                         , shouldBe
                                         , shouldContain
                                         , shouldNotContain
                                         , shouldSatisfy
                                         , shouldThrow
                                         )

import           Types.Chat              ( content )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Snapshot.Git" $ do
    describe "listProjects" $ do
      it "throws on invalid project-index.json" $ withTempCache $ \tempDir -> do
        let scopeRoot = "/tmp/telos-project"
            path      = indexPathFor tempDir scopeRoot
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path "{ invalid json"
        listProjects scopeRoot `shouldThrow` anyException

      it "reads project-index.json correctly" $ withTempCache $ \tempDir -> do
        let scopeRoot     = "/tmp/telos-project"
            path          = indexPathFor tempDir scopeRoot
            expectedNames = map Text.pack [ "demo", "work" ]
            indexJson
              = "{\n"
              <> "  \"projects\": {\n"
              <> "    \"demo\": { \"uuid\": \"id-a\", \"path\": \"/tmp/proj-a\", \"lastSession\": \"hash-a\" },\n"
              <> "    \"work\": { \"uuid\": \"id-b\", \"path\": \"/tmp/proj-b\", \"lastSession\": null }\n"
              <> "  }\n"
              <> "}\n"
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path indexJson
        entries <- listProjects scopeRoot
        let names = sortOn id (map (view projectName) entries)
        names `shouldBe` sortOn id expectedNames

    describe "createProject" $ do
      it "rejects duplicate project names" $ withTempCache $ \_ -> do
        let scopeRoot = "/tmp/telos-project"
        firstResult <- createProject scopeRoot "demo" "/tmp/proj"
        secondResult <- createProject scopeRoot "demo" "/tmp/proj"
        firstResult `shouldSatisfy` isRight
        secondResult `shouldSatisfy` isLeft

    describe "renameProject" $ do
      it "updates project name correctly" $ withTempCache $ \_ -> do
        let scopeRoot = "/tmp/telos-project"
        created <- createProject scopeRoot "untitled-1" "/tmp/proj"
        created `shouldSatisfy` isRight
        renamed <- renameProject scopeRoot "untitled-1" "demo"
        renamed `shouldSatisfy` isRight
        entries <- listProjects scopeRoot
        let names = map (view projectName) entries
        names `shouldContain` [ Text.pack "demo" ]
        names `shouldNotContain` [ Text.pack "untitled-1" ]

  describe "CLI.Context" $ do
    describe "buildContextMessage" $ do
      it "truncates content when exceeding maxPerFile" $ withTempDir $ \tempDir -> do
        let scopeRoot = tempDir
            filePath  = tempDir </> "snippet.txt"
        writeFile filePath "0123456789"
        let ctxSpec
              = defaultContextSpec & paths .~ [ "snippet.txt" ] & maxPerFile .~ 4 & maxBytes .~ 100
        message <- runM $ runFileSystemLocal scopeRoot $ buildContextMessage scopeRoot ctxSpec
        case message of
          Nothing  -> fail "Expected context message to be built"
          Just msg -> do
            let body = view content msg
            body `shouldSatisfy` Text.isInfixOf "... [truncated]"
            body `shouldSatisfy` Text.isInfixOf "# path: snippet.txt"

    describe "validatePathSpec" $ do
      it "rejects paths outside scope" $ withTempDir $ \tempDir -> do
        let scopeRoot = tempDir
        result <- validatePathSpec scopeRoot "/etc/passwd"
        result `shouldSatisfy` isLeft

  describe "Markdown.RenderAnsi" $ do
    describe "renderMarkdown" $ do
      it "renders headings with ANSI codes" $ do
        let linesOut = renderMarkdown "# Title\n"
        case linesOut of
          []       -> fail "Expected heading line"
          line : _ -> do
            stripAnsi line `shouldBe` "Title"
            line `shouldSatisfy` Text.isInfixOf "\ESC["

      it "renders list items" $ do
        let linesOut = renderMarkdown "- item\n"
        case linesOut of
          []       -> fail "Expected list line"
          line : _ -> stripAnsi line `shouldBe` "- item"

      it "renders blockquotes" $ do
        let linesOut = renderMarkdown "> q\n"
        case linesOut of
          []       -> fail "Expected quote line"
          line : _ -> stripAnsi line `shouldBe` "| q"

      it "renders code blocks preserving whitespace" $ do
        let linesOut = renderMarkdown "```\n  x\n```\n"
        linesOut `shouldSatisfy` any ((== "  x") . stripAnsi)

      it "renders inline formatting" $ do
        let linesOut = renderMarkdown "*b* _i_ `c`\n"
        case linesOut of
          []       -> fail "Expected inline line"
          line : _ -> stripAnsi line `shouldBe` "b i c"

      it "renders bold with underscores" $ do
        let linesOut = renderMarkdown "__bold__\n"
        case linesOut of
          []       -> fail "Expected bold line"
          line : _ -> do
            stripAnsi line `shouldBe` "bold"
            line `shouldSatisfy` Text.isInfixOf "\ESC["

      it "renders nested inline formatting" $ do
        let linesOut = renderMarkdown "**bold _italic_**\n"
        case linesOut of
          []       -> fail "Expected nested inline line"
          line : _ -> stripAnsi line `shouldBe` "bold italic"

      it "renders escaped inline markers" $ do
        let linesOut = renderMarkdown "\\*not italic\\* and \\_not\\_\n"
        case linesOut of
          []       -> fail "Expected escaped inline line"
          line : _ -> do
            stripAnsi line `shouldBe` "*not italic* and _not_"
            line `shouldSatisfy` (not . Text.isInfixOf "\ESC[")

      it "renders nested lists with indentation" $ do
        let linesOut = renderMarkdown "- Main\n  - Sub\n    - Nested\n"
        map stripAnsi linesOut `shouldBe` [ "- Main", "  - Sub", "    - Nested" ]

      it "renders list-like text inside quotes stably" $ do
        let linesOut = renderMarkdown "> - item\n"
        case linesOut of
          []       -> fail "Expected quoted line"
          line : _ -> do
            stripAnsi line `shouldBe` "| - item"
            line `shouldSatisfy` Text.isInfixOf "\ESC["

      it "keeps rendering as code for unclosed fences" $ do
        let linesOut = renderMarkdown "```python\nx\nstill\n"
        map stripAnsi linesOut `shouldBe` [ "x", "still" ]
        linesOut `shouldSatisfy` all (Text.isInfixOf "\ESC[")

  describe "Markdown.Stream" $ do
    describe "pushDelta" $ do
      it "does not output before newline" $ do
        let st0           = newStreamState
            ( st1, out1 ) = pushDelta st0 "Hello"
            ( _, out2 )   = pushDelta st1 "\nWorld"
        out1 `shouldBe` []
        map stripAnsi out2 `shouldBe` [ "Hello" ]

    describe "finalizeStream" $ do
      it "flushes remaining line" $ do
        let st0           = newStreamState
            ( st1, out1 ) = pushDelta st0 "Hello\nWorld"
            ( _, out2 )   = finalizeStream st1
            hasHello      = "Hello" `elem` map stripAnsi out1
            hasWorld      = "World" `elem` map stripAnsi out2
        hasHello `shouldBe` True
        hasWorld `shouldBe` True

      it "flushes incomplete last line inside unclosed fences" $ do
        let st0           = newStreamState
            ( st1, out1 ) = pushDelta st0 "```python\nx\nstill"
            ( _, out2 )   = finalizeStream st1
        map stripAnsi out1 `shouldBe` [ "x" ]
        map stripAnsi out2 `shouldBe` [ "still" ]
        out1 `shouldSatisfy` all (Text.isInfixOf "\ESC[")
        out2 `shouldSatisfy` all (Text.isInfixOf "\ESC[")

-- Helper functions

indexPathFor :: FilePath -> FilePath -> FilePath
indexPathFor cacheRoot scopeRoot
  = cacheRoot </> "telos" </> "project-index" </> Text.unpack (sha256Hex (Text.pack scopeRoot))
  <> ".json"

sha256Hex :: Text -> Text
sha256Hex payload
  = let
      digest   = hash (encodeUtf8 payload) :: Digest SHA256
      hexBytes = convertToBase Base16 digest
    in 
      Text.pack (BS8.unpack hexBytes)

withTempCache :: (FilePath -> IO ()) -> IO ()
withTempCache action = do
  oldCache <- lookupEnv "XDG_CACHE_HOME"
  tmpBase <- getTemporaryDirectory
  bracket (createTempDirectory tmpBase "telos-test-") (cleanup oldCache) $ \tempDir -> do
    setEnv "XDG_CACHE_HOME" tempDir
    action tempDir
  where
    cleanup oldCache tempDir = do
      removeDirectoryRecursive tempDir
      case oldCache of
        Just value -> setEnv "XDG_CACHE_HOME" value
        Nothing    -> unsetEnv "XDG_CACHE_HOME"

withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
  tmpBase <- getTemporaryDirectory
  bracket (createTempDirectory tmpBase "telos-test-") removeDirectoryRecursive action

stripAnsi :: Text -> Text
stripAnsi = Text.pack . stripAnsiChars . Text.unpack

stripAnsiChars :: [ Char ] -> [ Char ]
stripAnsiChars [] = []
stripAnsiChars ('\ESC' : '[' : rest) = case dropWhile (/= 'm') rest of
  []       -> []
  (_ : xs) -> stripAnsiChars xs
stripAnsiChars (c : cs) = c : stripAnsiChars cs
