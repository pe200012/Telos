{-# LANGUAGE OverloadedStrings #-}

module Provider.Lua
  ( ProviderError(..)
  , buildRequestBody
  , loadProviderSpec
  , providersDir
  , listProviders
  ) where

import           Control.Exception  ( IOException, catch )

import           Data.Aeson         ( Value(..), eitherDecodeStrict' )
import qualified Data.Aeson.Key     as AesonKey
import qualified Data.Aeson.KeyMap  as KeyMap
import qualified Data.Text          as Text
import           Data.Text.Encoding ( encodeUtf8 )

import           Provider.Spec      ( ProviderSpec )

import           Relude             hiding ( encodeUtf8 )

import           System.Directory   ( createDirectoryIfMissing
                                    , doesDirectoryExist
                                    , doesFileExist
                                    , findExecutable
                                    , getHomeDirectory
                                    , listDirectory
                                    )
import           System.Exit        ( ExitCode(..) )
import           System.FilePath    ( (</>), dropExtension, takeExtension )
import           System.Process     ( readProcessWithExitCode )

data ProviderError
  = ProviderScriptNotFound Text FilePath [ Text ]
  | ProviderLuaNotFound
  | ProviderLuaFailed Text
  | ProviderSpecDecodeFailed Text
  | ProviderRequestDecodeFailed Text
  deriving ( Eq, Show )

providersDir :: IO FilePath
providersDir = do
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let base = fromMaybe (home </> ".config") mXdg
  pure (base </> "telos" </> "providers")

listProviders :: IO [ Text ]
listProviders
  = do
    dir <- providersDir
    exists <- doesDirectoryExist dir
    if not exists
      then pure []
      else do
        files <- listDirectory dir
        let names = map Text.pack $ sort $ map dropExtension $ filter (\f -> takeExtension f
                                                                       == ".lua") files
        pure names

loadProviderSpec :: Text -> IO (Either ProviderError ProviderSpec)
loadProviderSpec providerName = do
  dir <- providersDir
  createDirectoryIfMissing True dir
  let scriptPath = dir </> (Text.unpack providerName <> ".lua")
  exists <- doesFileExist scriptPath
  if not exists
    then Left . ProviderScriptNotFound providerName scriptPath <$> listProviders
    else do
      interp <- findLuaInterpreter
      case interp of
        Nothing  -> pure (Left ProviderLuaNotFound)
        Just exe -> runLua exe scriptPath

buildRequestBody :: Text -> Value -> IO (Either ProviderError Value)
buildRequestBody providerName ctx = do
  dir <- providersDir
  createDirectoryIfMissing True dir
  let scriptPath = dir </> (Text.unpack providerName <> ".lua")
  exists <- doesFileExist scriptPath
  if not exists
    then Left . ProviderScriptNotFound providerName scriptPath <$> listProviders
    else do
      interp <- findLuaInterpreter
      case interp of
        Nothing  -> pure (Left ProviderLuaNotFound)
        Just exe -> do
          result <- runLuaValue exe scriptPath (luaBuildRequestRunner ctx)
          case result of
            Left err         -> pure (Left err)
            Right stdoutText -> case eitherDecodeStrict' (encodeUtf8 stdoutText) of
              Left decodeErr -> pure
                (Left (ProviderRequestDecodeFailed (Text.pack decodeErr <> "\n" <> stdoutText)))
              Right v        -> pure (Right v)

findLuaInterpreter :: IO (Maybe FilePath)
findLuaInterpreter = do
  lua <- findExecutable "lua"
  case lua of
    Just exe -> pure (Just exe)
    Nothing  -> findExecutable "luajit"

runLua :: FilePath -> FilePath -> IO (Either ProviderError ProviderSpec)
runLua exe scriptPath = do
  result <- runLuaValue exe scriptPath luaSpecRunner
  case result of
    Left err    -> pure (Left err)
    Right value -> case eitherDecodeStrict' (encodeUtf8 (Text.strip value)) of
      Left decodeErr -> pure
        (Left (ProviderSpecDecodeFailed (Text.pack decodeErr <> "\n" <> Text.strip value)))
      Right spec     -> pure (Right spec)

runLuaValue :: FilePath -> FilePath -> Text -> IO (Either ProviderError Text)
runLuaValue exe scriptPath runner
  = (do
       ( exitCode, out, err )
         <- readProcessWithExitCode exe [ "-", scriptPath ] (Text.unpack runner)
       case exitCode of
         ExitFailure n -> pure
           (Left
              (ProviderLuaFailed
                 ("ExitFailure " <> Text.pack (show n) <> "\n" <> Text.pack out <> Text.pack err)))
         ExitSuccess   -> do
           let stdoutText = Text.strip (Text.pack out)
           pure (Right stdoutText)) `catch` \(e :: IOException) -> pure
    (Left (ProviderLuaFailed (Text.pack (displayException e))))

luaJsonEncoder :: [ Text ]
luaJsonEncoder
  = [ "local QUOTE = string.char(34)"
    , "local function is_array(t)"
    , "  local n = 0"
    , "  for k, _ in pairs(t) do"
    , "    if type(k) ~= 'number' then return false end"
    , "    if k > n then n = k end"
    , "  end"
    , "  for i = 1, n do"
    , "    if t[i] == nil then return false end"
    , "  end"
    , "  return true"
    , "end"
    , "local function escape_str(s)"
    , "  s = s:gsub('\\\\', '\\\\\\\\')"
    , "  s = s:gsub(QUOTE, '\\\\' .. QUOTE)"
    , "  s = s:gsub('\\n', '\\\\n')"
    , "  s = s:gsub('\\r', '\\\\r')"
    , "  s = s:gsub('\\t', '\\\\t')"
    , "  return s"
    , "end"
    , "local function encode(v)"
    , "  local tv = type(v)"
    , "  if tv == 'nil' then return 'null' end"
    , "  if tv == 'boolean' then return v and 'true' or 'false' end"
    , "  if tv == 'number' then return tostring(v) end"
    , "  if tv == 'string' then return QUOTE .. escape_str(v) .. QUOTE end"
    , "  if tv == 'table' then"
    , "    if is_array(v) then"
    , "      local out = {}"
    , "      for i = 1, #v do out[#out+1] = encode(v[i]) end"
    , "      return '[' .. table.concat(out, ',') .. ']'"
    , "    else"
    , "      local out = {}"
    , "      for k, vv in pairs(v) do"
    , "        if type(k) ~= 'string' then error('object key must be string') end"
    , "        out[#out+1] = encode(k) .. ':' .. encode(vv)"
    , "      end"
    , "      return '{' .. table.concat(out, ',') .. '}'"
    , "    end"
    , "  end"
    , "  error('unsupported type: ' .. tv)"
    , "end"
    ]

luaSpecRunner :: Text
luaSpecRunner
  = Text.unlines
    ([ "local path = assert(arg[1], 'provider script path is required')"
     , "local spec = dofile(path)"
     , "if type(spec) ~= 'table' then error('provider script must return a table') end"
     , "local required = {"
     , "  'api_key_env',"
     , "  'default_base_url',"
     , "  'default_model',"
     , "  'default_temperature',"
     , "  'auth_header',"
     , "  'auth_prefix',"
     , "  'supports_tool_calls',"
     , "}"
     , "for _, k in ipairs(required) do"
     , "  if spec[k] == nil then error('missing key: ' .. k) end"
     , "end"
     , "local out = {}"
     , "for _, k in ipairs(required) do out[k] = spec[k] end"
     ]
     <> luaJsonEncoder
     <> [ "io.write(encode(out))" ])

luaBuildRequestRunner :: Value -> Text
luaBuildRequestRunner ctx
  = let
      ctxExpr = luaExpr ctx
    in 
      Text.unlines
        ([ "local path = assert(arg[1], 'provider script path is required')"
         , "local spec = dofile(path)"
         , "if type(spec) ~= 'table' then error('provider script must return a table') end"
         , "local ctx = " <> ctxExpr
         , "local fn = spec.build_request"
         , "if type(fn) ~= 'function' then error('missing build_request(ctx)') end"
         , "local body = fn(ctx)"
         , "if type(body) ~= 'table' then error('build_request must return a table') end"
         ]
         <> luaJsonEncoder
         <> [ "io.write(encode(body))" ])

luaExpr :: Value -> Text
luaExpr = \case
  Object obj -> let
      pairs    = KeyMap.toList obj
      rendered = map renderPair pairs
    in 
      "{" <> Text.intercalate "," rendered <> "}"
  Array arr  -> let
      xs       = toList arr
      rendered = map luaExpr xs
    in 
      "{" <> Text.intercalate "," rendered <> "}"
  String t   -> luaString t
  Number n   -> Text.pack (show n)
  Bool True  -> "true"
  Bool False -> "false"
  Null       -> "nil"
  where
    renderPair ( k, v ) = "[" <> luaString (AesonKey.toText k) <> "]=" <> luaExpr v

luaString :: Text -> Text
luaString t = "\"" <> escapeLuaString t <> "\""

escapeLuaString :: Text -> Text
escapeLuaString = Text.concatMap $ \case
  '\\' -> "\\\\"
  '"'  -> "\\\""
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c    -> Text.singleton c
