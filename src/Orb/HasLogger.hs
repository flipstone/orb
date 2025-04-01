module Orb.HasLogger
  ( HasLogger (..)
  ) where

import Control.Exception (SomeException)

class HasLogger m where
  log :: SomeException -> m ()
