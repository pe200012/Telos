module Main ( main ) where

import           CLI.Context             ( buildContextMessage
                                         , defaultContextSpec
                                         , maxBytes
                                         , maxPerFile
                                         , paths
                                         , validatePathSpec
                                         )

import           Control.Exception       ( SomeException, bracket, try )
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

import           Types.Chat              ( content )

main :: IO ()
main = do
  ok1 <- testInvalidProjectIndexThrows
  ok2 <- testListProjectsReadsIndex
  ok3 <- testCreateProjectRejectsDuplicate
  ok4 <- testRenameProject
  ok5 <- testContextBuildTruncates
  ok6 <- testValidatePathSpecOutsideScope
  ok7 <- testRenderHeading
  ok8 <- testRenderList
  ok9 <- testRenderQuote
  ok10 <- testRenderCodeBlock
  ok11 <- testRenderInline
  ok12 <- testRenderBoldUnderscore
  ok13 <- testRenderNestedInline
  ok14 <- testRenderEscapedInline
  ok15 <- testStreamIncremental
  ok16 <- testStreamFinalize
  if and [ ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11, ok12, ok13, ok14, ok15, ok16 ]
    then putStrLn "All tests passed"
    else exitFailure

testInvalidProjectIndexThrows :: IO Bool
testInvalidProjectIndexThrows = withTempCache $ \tempDir -> do
  let scopeRoot = "/tmp/telos-project"
      path      = indexPathFor tempDir scopeRoot
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path "{ invalid json"
  result <- try (void (listProjects scopeRoot)) :: IO (Either SomeException ())
  case result of
    Left _  -> pure True
    Right _ -> do
      putStrLn "Expected invalid project-index.json to raise an exception"
      pure False

testListProjectsReadsIndex :: IO Bool
testListProjectsReadsIndex = withTempCache $ \tempDir -> do
  let scopeRoot = "/tmp/telos-project"
      path      = indexPathFor tempDir scopeRoot
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path indexJson
  entries <- listProjects scopeRoot
  let names = sortOn id (map (view projectName) entries)
  if names == sortOn id expectedNames
    then pure True
    else do
      putStrLn ("Expected names " <> show expectedNames <> ", got " <> show names)
      pure False
  where
    expectedNames = map Text.pack [ "demo", "work" ]

    indexJson
      = "{\n"
      <> "  \"projects\": {\n"
      <> "    \"demo\": { \"uuid\": \"id-a\", \"path\": \"/tmp/proj-a\", \"lastSession\": \"hash-a\" },\n"
      <> "    \"work\": { \"uuid\": \"id-b\", \"path\": \"/tmp/proj-b\", \"lastSession\": null }\n"
      <> "  }\n"
      <> "}\n"

testCreateProjectRejectsDuplicate :: IO Bool
testCreateProjectRejectsDuplicate = withTempCache $ \_ -> do
  let scopeRoot = "/tmp/telos-project"
  firstResult <- createProject scopeRoot "demo" "/tmp/proj"
  secondResult <- createProject scopeRoot "demo" "/tmp/proj"
  case ( firstResult, secondResult ) of
    ( Right _, Left _ ) -> pure True
    _ -> do
      putStrLn "Expected duplicate project name to be rejected"
      pure False

testRenameProject :: IO Bool
testRenameProject = withTempCache $ \_ -> do
  let scopeRoot = "/tmp/telos-project"
  created <- createProject scopeRoot "untitled-1" "/tmp/proj"
  case created of
    Left err -> do
      putStrLn ("Unexpected create failure: " <> Text.unpack err)
      pure False
    Right _  -> do
      renamed <- renameProject scopeRoot "untitled-1" "demo"
      case renamed of
        Left err -> do
          putStrLn ("Unexpected rename failure: " <> Text.unpack err)
          pure False
        Right _  -> do
          entries <- listProjects scopeRoot
          let names = map (view projectName) entries
          if Text.pack "demo" `elem` names && Text.pack "untitled-1" `notElem` names
            then pure True
            else do
              putStrLn "Expected rename to update project name"
              pure False

testContextBuildTruncates :: IO Bool
testContextBuildTruncates = withTempDir $ \tempDir -> do
  let scopeRoot = tempDir
      filePath  = tempDir </> "snippet.txt"
  writeFile filePath "0123456789"
  let spec = defaultContextSpec & paths .~ [ "snippet.txt" ] & maxPerFile .~ 4 & maxBytes .~ 100
  message <- runM $ runFileSystemLocal scopeRoot $ buildContextMessage scopeRoot spec
  case message of
    Nothing  -> do
      putStrLn "Expected context message to be built"
      pure False
    Just msg -> do
      let body = view content msg
      if "... [truncated]" `Text.isInfixOf` body && "# path: snippet.txt" `Text.isInfixOf` body
        then pure True
        else do
          putStrLn "Expected truncated content and path marker in context"
          pure False

testValidatePathSpecOutsideScope :: IO Bool
testValidatePathSpecOutsideScope = withTempDir $ \tempDir -> do
  let scopeRoot = tempDir
  result <- validatePathSpec scopeRoot "/etc/passwd"
  case result of
    Left _  -> pure True
    Right _ -> do
      putStrLn "Expected outside path to be rejected"
      pure False

testRenderHeading :: IO Bool
testRenderHeading = do
  let linesOut = renderMarkdown "# Title\n"
  case linesOut of
    []       -> do
      putStrLn "Expected heading line"
      pure False
    line : _ -> do
      let stripped = stripAnsi line
      if stripped == "Title" && Text.isInfixOf "\ESC[" line
        then pure True
        else do
          putStrLn ("Unexpected heading render: " <> Text.unpack stripped)
          pure False

testRenderList :: IO Bool
testRenderList = do
  let linesOut = renderMarkdown "- item\n"
  case linesOut of
    []       -> do
      putStrLn "Expected list line"
      pure False
    line : _ -> if stripAnsi line == "- item"
      then pure True
      else do
        putStrLn "Unexpected list render"
        pure False

testRenderQuote :: IO Bool
testRenderQuote = do
  let linesOut = renderMarkdown "> q\n"
  case linesOut of
    []       -> do
      putStrLn "Expected quote line"
      pure False
    line : _ -> if stripAnsi line == "| q"
      then pure True
      else do
        putStrLn "Unexpected quote render"
        pure False

testRenderCodeBlock :: IO Bool
testRenderCodeBlock = do
  let linesOut = renderMarkdown "```\n  x\n```\n"
  if any ((== "  x") . stripAnsi) linesOut
    then pure True
    else do
      putStrLn "Expected code line to preserve whitespace"
      pure False

testRenderInline :: IO Bool
testRenderInline = do
  let linesOut = renderMarkdown "*b* _i_ `c`\n"
  case linesOut of
    []       -> do
      putStrLn "Expected inline line"
      pure False
    line : _ -> if stripAnsi line == "b i c"
      then pure True
      else do
        putStrLn "Unexpected inline render"
        pure False

testRenderBoldUnderscore :: IO Bool
testRenderBoldUnderscore = do
  let linesOut = renderMarkdown "__bold__\n"
  case linesOut of
    []       -> do
      putStrLn "Expected bold line"
      pure False
    line : _ -> if stripAnsi line == "bold" && Text.isInfixOf "\ESC[" line
      then pure True
      else do
        putStrLn "Unexpected bold underscore render"
        pure False

testRenderNestedInline :: IO Bool
testRenderNestedInline = do
  let linesOut = renderMarkdown "**bold _italic_**\n"
  case linesOut of
    []       -> do
      putStrLn "Expected nested inline line"
      pure False
    line : _ -> if stripAnsi line == "bold italic"
      then pure True
      else do
        putStrLn "Unexpected nested inline render"
        pure False

testRenderEscapedInline :: IO Bool
testRenderEscapedInline = do
  let linesOut = renderMarkdown "\\*not italic\\* and \\_not\\_\n"
  case linesOut of
    []       -> do
      putStrLn "Expected escaped inline line"
      pure False
    line : _ -> if stripAnsi line == "*not italic* and _not_" && not (Text.isInfixOf "\ESC[" line)
      then pure True
      else do
        putStrLn "Unexpected escaped inline render"
        pure False

testStreamIncremental :: IO Bool
testStreamIncremental = do
  let st0           = newStreamState
      ( st1, out1 ) = pushDelta st0 "Hello"
      ( _, out2 )   = pushDelta st1 "\nWorld"
  if not (null out1)
    then do
      putStrLn "Expected no output before newline"
      pure False
    else if map stripAnsi out2 == [ "Hello" ]
      then pure True
      else do
        putStrLn "Unexpected incremental output"
        pure False

testStreamFinalize :: IO Bool
testStreamFinalize = do
  let st0           = newStreamState
      ( st1, out1 ) = pushDelta st0 "Hello\nWorld"
      ( _, out2 )   = finalizeStream st1
      hasHello      = "Hello" `elem` map stripAnsi out1
      hasWorld      = "World" `elem` map stripAnsi out2
  if hasHello && hasWorld
    then pure True
    else do
      putStrLn "Expected finalize to flush remaining line"
      pure False

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

withTempCache :: (FilePath -> IO Bool) -> IO Bool
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

withTempDir :: (FilePath -> IO Bool) -> IO Bool
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
