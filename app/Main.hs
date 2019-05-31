module Main where

import System.Environment
import System.Posix.PtyWrapper

main = do
  args <- getArgs
  case args of
    (_procTitle:sockPath:executable:args) -> spawnWithPtyWrapper sockPath executable args
    _                                     -> error "Usage: pty-wrapper-exe <processTitle> <socketPath> <executable> <args>"
