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
    [ TastyHH.testProperty "cannot generate an unknown open api" prop_openApiUnknownLabel
    , test_simpleGet
    , test_simplePost
    , test_getWithQuery
    , test_getWithHeaders
    , test_getWithCookies
    , test_openApiSubset
    , test_nullableRefOpenApi
    , test_unionOpenApi
    , test_nullableRefCollectComponentsOpenApi
    ]

prop_openApiUnknownLabel :: HH.Property
prop_openApiUnknownLabel = HH.withTests 1 . HH.property $ do
  case Orb.mkOpenApi Fixtures.simpleGetOpenApiRouter "unknown-open-api" of
    Right _ -> fail "Should not have returned an OpenApi for an unknown label"
    Left msg -> msg === "No OpenApi definition found with label unknown-open-api."

test_simpleGet :: Tasty.TestTree
test_simpleGet =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a simple get"
    "test/examples/simple-get.json"
    $ Orb.mkOpenApi Fixtures.simpleGetOpenApiRouter "simple-get"

test_simplePost :: Tasty.TestTree
test_simplePost =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a simple post"
    "test/examples/simple-post.json"
    $ Orb.mkOpenApi Fixtures.simplePostOpenApiRouter "simple-post"

test_getWithQuery :: Tasty.TestTree
test_getWithQuery =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with query params"
    "test/examples/get-with-query.json"
    $ Orb.mkOpenApi Fixtures.getWithQueryOpenApiRouter "get-with-query"

test_getWithHeaders :: Tasty.TestTree
test_getWithHeaders =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with header params"
    "test/examples/get-with-headers.json"
    $ Orb.mkOpenApi Fixtures.getWithHeadersOpenApiRouter "get-with-headers"

test_getWithCookies :: Tasty.TestTree
test_getWithCookies =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with header params"
    "test/examples/get-with-cookies.json"
    $ Orb.mkOpenApi Fixtures.getWithCookiesOpenApiRouter "get-with-cookies"

test_openApiSubset :: Tasty.TestTree
test_openApiSubset =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a subset of routes"
    "test/examples/open-api-subset.json"
    $ Orb.mkOpenApi Fixtures.openApiSubsetRouter "open-api-subset"

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

test_nullableRefCollectComponentsOpenApi :: Tasty.TestTree
test_nullableRefCollectComponentsOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a nullable schema with an inner object schema"
    "test/examples/nullable-ref-collect-components.json"
    $ Orb.mkOpenApi Fixtures.nullableRefCollectComponentsOpenApiRouter "nullable-ref-collect-components"

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
