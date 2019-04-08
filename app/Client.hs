module Main where

import System.Environment
import System.Posix.PtyWrapper

main = do
  args <- getArgs
  case args of
    [sockPath] -> ptyWrapperClient sockPath
    _          -> error "Usage: pty-wrapper-client <socketPath>"
