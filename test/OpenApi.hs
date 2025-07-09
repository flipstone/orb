{-# LANGUAGE TemplateHaskell #-}

module OpenApi
  ( testGroup
  ) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy as LBS
import Data.OpenApi qualified as OpenApi
import Hedgehog ((===))
import Hedgehog qualified as HH
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Hedgehog qualified as TastyHH

import Fixtures qualified
import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "OpenApi"
    [ test_openApi
    , TastyHH.testProperty "cannot generate an unknown open api" prop_openApiUnknownLabel
    , test_openApiSubset
    , test_nullableRefOpenApi
    , test_unionOpenApi
    ]

test_openApi :: Tasty.TestTree
test_openApi =
  mkGoldenTest
    "can generate a requested open api json"
    "test/examples/basic-open-api.json"
    $ Orb.mkOpenApi Fixtures.basicOpenApiRouter "basic-open-api"

prop_openApiUnknownLabel :: HH.Property
prop_openApiUnknownLabel = HH.withTests 1 . HH.property $ do
  case Orb.mkOpenApi Fixtures.basicOpenApiRouter "unknown-open-api" of
    Right _ -> fail "Should not have returned an OpenApi for an unknown label"
    Left msg -> msg === "No OpenApi definition found with label unknown-open-api."

test_openApiSubset :: Tasty.TestTree
test_openApiSubset =
  mkGoldenTest
    "can generate a requested open api json for a subset of routes"
    "test/examples/just-route-1.json"
    $ Orb.mkOpenApi Fixtures.basicOpenApiRouter "just-route-1"

test_nullableRefOpenApi :: Tasty.TestTree
test_nullableRefOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a nullable schema"
    "test/examples/nullable-ref.json"
    $ Orb.mkOpenApi Fixtures.nullableRefOpenApiRouter "nullable-ref"

test_unionOpenApi :: Tasty.TestTree
test_unionOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a union schema"
    "test/examples/union.json"
    $ Orb.mkOpenApi Fixtures.unionOpenApiRouter "union"

mkGoldenTest ::
  Tasty.TestName ->
  FilePath ->
  Either String OpenApi.OpenApi ->
  Tasty.TestTree
mkGoldenTest testName goldenPath eopenApi = do
  -- Using VsStringDiff instead of VsString because the output for failing
  -- tests is better
  goldenVsStringDiff testName (\ref new -> ["diff", "-u", ref, new]) goldenPath $ do
    openApi <- either fail pure eopenApi
    -- Aeson Pretty doesn't emit a newline at the end, but some text editors
    -- like to add it. So we explicitly add it.
    pure $ AesonPretty.encodePretty openApi <> LBS.pack [10]
