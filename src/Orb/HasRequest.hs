module Orb.HasRequest
  ( HasRequest (..)
  )
where

import Network.Wai qualified as Wai

class HasRequest m where
  request :: m Wai.Request
