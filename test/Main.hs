module Main
  ( main
  ) where

import Test.Tasty qualified as Tasty

import OpenApi qualified
import SwaggerUI qualified

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Orb"
      [ OpenApi.testGroup
      , SwaggerUI.testGroup
      ]
