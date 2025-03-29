module Orb.Response.ContentType
  ( ContentType (..)
  , contentTypeToBytes
  ) where

import Data.ByteString.Char8 qualified as BS8

data ContentType
  = CSV
  | HTML
  | JSON
  | PlainText
  | XML

contentTypeToBytes :: ContentType -> BS8.ByteString
contentTypeToBytes contentType =
  BS8.pack $
    case contentType of
      CSV -> "text/csv"
      HTML -> "text/html"
      JSON -> "application/json"
      PlainText -> "text/plain"
      XML -> "text/xml"
