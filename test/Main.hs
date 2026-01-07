module Main
  ( main
  ) where

import Test.Tasty qualified as Tasty

import Form qualified
import Handler qualified
import OpenApi qualified
import SwaggerUI qualified

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Orb"
      [ Form.testGroup
      , Handler.testGroup
      , OpenApi.testGroup
      , SwaggerUI.testGroup
      ]
