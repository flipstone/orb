module Main
  ( main
  ) where

import Test.Tasty qualified as Tasty

import Handler qualified
import OpenApi qualified
import SwaggerUI qualified

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Orb"
      [ Handler.testGroup
      , OpenApi.testGroup
      , SwaggerUI.testGroup
      ]
