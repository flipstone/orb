module Orb.Response.Document
  ( Document (..)
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

data Document = Document
  { documentFileName :: BS.ByteString
  , documentContent :: LBS.ByteString
  , documentType :: BS.ByteString
  }
