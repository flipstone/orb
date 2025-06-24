{-# LANGUAGE TemplateHaskell #-}

module OpenApi
  ( testGroup
  ) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy as LBS
import Data.FileEmbed qualified as FileEmbed
import Data.OpenApi qualified as OpenApi
import Hedgehog ((===))
import Hedgehog qualified as HH
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Fixtures qualified
import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "OpenApi"
    [ TastyHH.testProperty "can generate a requested open api json" prop_openApi
    , TastyHH.testProperty "cannot generate an unknown open api" prop_openApiUnknownLabel
    , TastyHH.testProperty "can generate a requested open api json for a subset of routes" prop_openApiSubset
    ]

prop_openApi :: HH.Property
prop_openApi = HH.withTests 1 . HH.property $ do
  openApi <- HH.evalEither (Orb.mkOpenApi Fixtures.basicOpenApiRouter "basic-open-api")
  assertEqualOpenApi openApi $(FileEmbed.embedFile "test/examples/basic-open-api.json")

prop_openApiUnknownLabel :: HH.Property
prop_openApiUnknownLabel = HH.withTests 1 . HH.property $ do
  case Orb.mkOpenApi Fixtures.basicOpenApiRouter "unknown-open-api" of
    Right _ -> fail "Should not have returned an OpenApi for an unknown label"
    Left msg -> msg === "No OpenApi definition found with label unknown-open-api."

prop_openApiSubset :: HH.Property
prop_openApiSubset = HH.withTests 1 . HH.property $ do
  openApi <- HH.evalEither (Orb.mkOpenApi Fixtures.basicOpenApiRouter "just-route-1")
  assertEqualOpenApi openApi $(FileEmbed.embedFile "test/examples/just-route-1.json")

assertEqualOpenApi :: HH.MonadTest m => OpenApi.OpenApi -> BS8.ByteString -> m ()
assertEqualOpenApi openApi expectedBytes = do
  -- Splitting on lines here both produces a more useful diff and allows for small
  -- meaningless differences such as newline at end of file between the encoded
  -- bytes and the expected bytes read from the sample file
  BS8.lines (LBS.toStrict (AesonPretty.encodePretty openApi))
    === BS8.lines expectedBytes
