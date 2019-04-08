module Main where

import System.Environment
import System.Posix.PtyWrapper

main = do
  args <- getArgs
  case args of
    [sockPath, executable] -> spawnWithPtyWrapper sockPath executable
    _                      -> error "Usage: pty-wrapper-exe <socketPath> <executable>"
