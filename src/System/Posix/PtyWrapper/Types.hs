{-# LANGUAGE TemplateHaskell #-}
module System.Posix.PtyWrapper.Types where

import Data.Aeson.TH
import Data.Text

data Event = Event {
    keys :: Maybe Text -- ^ Keys to forward to application
  , rows :: Maybe Int  -- ^ New count of pseudoterminal columns
  , cols :: Maybe Int  -- ^ New count of pseudoterminal rows
  } deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Event)

-- |Whether we look for executable in PATH
data SearchPath = DoSearchPath | DontSearchPath
  deriving (Eq)

-- |Whether to restrict socket access to root user (UID 0) only
data RestrictRoot = DoRestrictRoot | DontRestrictRoot
  deriving (Eq)
