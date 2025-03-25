module Orb.HasRespond
  ( HasRespond (..)
  )
where

import Network.Wai qualified as Wai

class HasRespond m where
  respond :: m (Wai.Response -> IO Wai.ResponseReceived)
