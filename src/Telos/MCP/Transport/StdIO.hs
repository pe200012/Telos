module Telos.MCP.Transport.StdIO
  ( StdIOHandle(..)
  , spawnMCPProcess
  , sendMessage
  , receiveMessage
  , receiveRawMessage
  , sendRawBytes
  , closeHandle
  ) where

import           Control.Concurrent.MVar
import           Control.Exception          ( SomeException, try )

import           Data.Aeson                 ( FromJSON, ToJSON, eitherDecode, encode )
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import           GHC.IO.Handle              ( hFlush )

import           System.IO                  ( BufferMode(..)
                                            , Handle
                                            , hClose
                                            , hGetLine
                                            , hSetBuffering
                                            )
import           System.Process             ( CreateProcess(..)
                                            , ProcessHandle
                                            , StdStream(..)
                                            , createProcess
                                            , proc
                                            , terminateProcess
                                            , waitForProcess
                                            )

import           Telos.Core.Error           ( MCPError(..) )
import           Telos.MCP.JsonRpc          ( JsonRpcMessage, decodeMessage )

data StdIOHandle
  = StdIOHandle { shProcess :: ProcessHandle
                , shStdin   :: Handle
                , shStdout  :: Handle
                , shStderr  :: Handle
                , shLock    :: MVar ()
                }

spawnMCPProcess :: FilePath
                -> [ String ]
                -> Maybe FilePath
                -> Maybe [ ( String, String ) ]
                -> IO (Either MCPError StdIOHandle)
spawnMCPProcess cmd args workDir env = do
  let cp
        = (proc cmd args) { std_in  = CreatePipe
                          , std_out = CreatePipe
                          , std_err = CreatePipe
                          , cwd     = workDir
                          , env     = env
                          }

  result <- try $ createProcess cp

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionFailed $ T.pack $ show e
    Right ( Just stdin, Just stdout, Just stderr, ph ) -> do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering
      hSetBuffering stderr NoBuffering

      lock <- newMVar ()

      pure
        $ Right
          StdIOHandle
          { shProcess = ph, shStdin = stdin, shStdout = stdout, shStderr = stderr, shLock = lock }
    Right _ -> pure $ Left $ MCPConnectionFailed "Failed to create process pipes"

sendMessage :: (ToJSON a) => StdIOHandle -> a -> IO (Either MCPError ())
sendMessage handle msg = do
  let payload = BL.toStrict $ encode msg
  result <- try $ withMVar (shLock handle) $ \_ -> do
    BL8.hPutStrLn (shStdin handle) (encode msg)
    hFlush (shStdin handle)

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionFailed $ T.pack $ show e
    Right () -> pure $ Right ()

receiveMessage :: (FromJSON a) => StdIOHandle -> IO (Either MCPError a)
receiveMessage handle = do
  result <- try $ hGetLine (shStdout handle)

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionFailed $ T.pack $ show e
    Right line -> case eitherDecode (BL8.pack line) of
      Left err  -> pure $ Left $ MCPProtocolError (-32700) $ T.pack err
      Right msg -> pure $ Right msg

closeHandle :: StdIOHandle -> IO ()
closeHandle handle = do
  _ <- try @SomeException $ hClose (shStdin handle)
  _ <- try @SomeException $ hClose (shStdout handle)
  _ <- try @SomeException $ hClose (shStderr handle)
  _ <- try @SomeException $ terminateProcess (shProcess handle)
  _ <- try @SomeException $ waitForProcess (shProcess handle)
  pure ()

-- | Receive and parse as JsonRpcMessage (unified type)
receiveRawMessage :: StdIOHandle -> IO (Either String JsonRpcMessage)
receiveRawMessage handle = do
  result <- try $ hGetLine (shStdout handle)
  case result of
    Left (e :: SomeException) -> pure $ Left $ show e
    Right line -> pure $ decodeMessage (BL8.pack line)

-- | Send raw bytes directly
sendRawBytes :: StdIOHandle -> BL.ByteString -> IO (Either MCPError ())
sendRawBytes handle bytes = do
  result <- try $ withMVar (shLock handle) $ \_ -> do
    BL8.hPutStrLn (shStdin handle) bytes
    hFlush (shStdin handle)
  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionFailed $ T.pack $ show e
    Right () -> pure $ Right ()
