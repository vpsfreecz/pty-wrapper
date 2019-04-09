module System.Posix.PtyWrapper (
  module System.Posix.PtyWrapper.Types
  , spawnWithPtyWrapper
  , spawnWithPtyWrapperOpts
  , ptyWrapperClient
  ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Aeson

import System.Posix.Pty
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text              as T

import System.Posix.PtyWrapper.Types

-- socket
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

-- |Spawn a process with PTY wrapper
-- so we can talk to it over UNIX domain sockets
spawnWithPtyWrapper :: FilePath -- ^ Socket target path
                    -> FilePath -- ^ Path of the executable
                    -> [String] -- ^ Command line arguments
                    -> IO ()
spawnWithPtyWrapper sockPath procName args =
  spawnWithPtyWrapperOpts
    sockPath
    procName
    args
    Nothing
    (80, 40)
    DoSearchPath
    DoRestrictRoot

-- |Like `spawnWithPtyWrapper` with customization of starting conditions,
-- like arguments, environment or initial window size
spawnWithPtyWrapperOpts :: FilePath                 -- ^ Socket target path
                        -> FilePath                 -- ^ Path of the executable
                        -> [String]                 -- ^ Command line arguments
                        -> Maybe [(String, String)] -- ^ Optional environment for the new process
                        -> (Int, Int)               -- ^ Initial dimensions for the pseudo terminal
                        -> SearchPath               -- ^ Search for executable in PATH
                        -> RestrictRoot             -- ^ Restrict access to socket to root (UID 0) only
                        -> IO ()
spawnWithPtyWrapperOpts sockPath procName procArgs extraEnviron (x, y) doSearchPath doRestrictRoot = do
  (pty, _handle) <- spawnWithPty extraEnviron (doSearchPath == DoSearchPath) procName procArgs (x, y)

  readQ <- atomically $ newTQueue
  cmdQ  <- atomically $ newTQueue

  runConcurrently $  Concurrently (ptyReader pty readQ)
                  *> Concurrently (ptyWriter pty cmdQ)
                  *> Concurrently (srv sockPath doRestrictRoot readQ cmdQ)

-- |Accept UNIX domain connections and forward data appropriately
srv :: String -> RestrictRoot -> TQueue BSC.ByteString -> TQueue Event -> IO ()
srv sockPath doRestrictRoot readQueue cmdQueue = bracket (open sockPath) close loop
  where
    open sockPath' = do
      sock <- socket AF_UNIX Stream 0
      bind sock (SockAddrUnix sockPath')
      listen sock 1
      return sock
    loop sock = do
      forever $ do
        (conn, _) <- accept sock
        (_pid, uid, _gid) <- getPeerCredential conn
        let mainAct = void $ forkFinally (
              runConcurrently $  Concurrently (toClient   conn readQueue)
                              *> Concurrently (fromClient conn cmdQueue)
                              ) (\_ -> close conn)

        case (doRestrictRoot == DoRestrictRoot, uid) of
          (True, Just 0) -> mainAct
          (False, _)     -> mainAct
          (True, _)      -> do
            putStrLn $ "Restricted UID: " ++ show uid
            close conn

    toClient conn readQ = forever $ do
      msg <- atomically $ readTQueue readQ
      sendAll conn msg

    fromClient conn cmdQ = forever $ do
      msg <- recv conn 4096
      unless (BSC.null msg) $ do
        case (eitherDecode $ BSL.fromStrict msg) of
          Left err -> error $ "JSON parse error: " ++ err
          Right evt -> atomically $ writeTQueue cmdQ $ evt

-- |Read PTY and forward ByteStrings to queue
ptyReader :: Pty -> TQueue BSC.ByteString -> IO ()
ptyReader pty readQ = forever $ do
    res <- tryReadPty pty
    case res of
      Left _err -> return ()
      Right msg -> do
        atomically $ writeTQueue readQ msg

-- |Read event queue and write to PTY
ptyWriter :: Pty -> TQueue Event -> IO ()
ptyWriter pty cmdQ = forever $ do
  cmd <- atomically $ readTQueue cmdQ
  case (keys cmd) of
    Nothing -> return ()
    Just xs -> writePty pty $ B64.decodeLenient $ BSC.pack $ T.unpack xs

  case (rows cmd, cols cmd) of
    (Just rs, Just cs) -> do
      (currentRows, currentCols) <- ptyDimensions pty
      when (rs /= currentRows || cs /= currentCols) $ resizePty pty (rs, cs)

    _ -> return ()

-- |Client for testing purposes
--
-- Reads lines from standard input and sends them as keys to wrapped process.
-- If line starts with ! it sends a resize request to resize PTY of the
-- wrapped process to 10 20
ptyWrapperClient :: String -> IO ()
ptyWrapperClient sockPath = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix sockPath)

  void $ forkIO $ forever $ do
    l <- getLine
    let b64l = T.pack . BSC.unpack . B64.encode . BSC.pack $ l
    case l of
      ('!':_) -> sendAll sock (BSL.toStrict $ encode $ Event Nothing (Just 10) (Just 20))
      _       -> sendAll sock (BSL.toStrict $ encode $ Event (Just b64l) Nothing Nothing)

  void $ forever $ do
    msg <- recv sock 4096
    unless (BSC.null msg) $ do
      BSC.putStr msg
  close sock
