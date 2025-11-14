module OpenApi
  ( testGroup
  ) where

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LBS
import Data.OpenApi qualified as OpenApi
import Data.Set qualified as Set
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
    [ test_openApiUnknownLabel
    , test_rejectsSchemaNamesWithUnallowedCharacters
    , test_simpleGet
    , test_simplePost
    , test_getWithQuery
    , test_getWithHeaders
    , test_getWithCookies
    , test_openApiSubset
    , test_nullableRefOpenApi
    , test_unionOpenApi
    , test_taggedUnionOpenApi
    , test_nullableRefCollectComponentsOpenApi
    ]

test_openApiUnknownLabel :: Tasty.TestTree
test_openApiUnknownLabel =
  TastyHH.testProperty "cannot generate an unknown open api" . HH.withTests 1 . HH.property $
    case mkTestOpenApi Fixtures.simpleGetOpenApiRouter "unknown-open-api" of
      Right _ -> fail "Should not have returned an OpenApi for an unknown label"
      Left msg ->
        fmap Orb.renderOpenApiError msg
          === ["No OpenApi definition found with label unknown-open-api."]

test_rejectsSchemaNamesWithUnallowedCharacters :: Tasty.TestTree
test_rejectsSchemaNamesWithUnallowedCharacters =
  TastyHH.testProperty "Rejects schema names with unallowed characters" . HH.withTests 1 . HH.property $
    let
      options =
        Orb.defaultOpenApiOptions
          { Orb.openApiAllowedSchemaNameChars = Set.fromList ""
          }
    in
      case Orb.mkOpenApi options Fixtures.simpleGetOpenApiRouter "simple-get" of
        Right _ -> fail "Should not have returned an OpenApi for an unknown label"
        Left errs ->
          fmap Orb.renderOpenApiError errs
            === [ "Invalid Schema Name: \"InternalServerError\" only the following characters are allowed, \"\".\n\
                  \  - Found at: Schema <<toplevel>>\n"
                , "Invalid Schema Name: \"SuccessMessage\" only the following characters are allowed, \"\".\n\
                  \  - Found at: Schema <<toplevel>>\n"
                ]

test_simpleGet :: Tasty.TestTree
test_simpleGet =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a simple get"
    "test/examples/simple-get.json"
    $ mkTestOpenApi Fixtures.simpleGetOpenApiRouter "simple-get"

test_simplePost :: Tasty.TestTree
test_simplePost =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a simple post"
    "test/examples/simple-post.json"
    $ mkTestOpenApi Fixtures.simplePostOpenApiRouter "simple-post"

test_getWithQuery :: Tasty.TestTree
test_getWithQuery =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with query params"
    "test/examples/get-with-query.json"
    $ mkTestOpenApi Fixtures.getWithQueryOpenApiRouter "get-with-query"

test_getWithHeaders :: Tasty.TestTree
test_getWithHeaders =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with header params"
    "test/examples/get-with-headers.json"
    $ mkTestOpenApi Fixtures.getWithHeadersOpenApiRouter "get-with-headers"

test_getWithCookies :: Tasty.TestTree
test_getWithCookies =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a get with header params"
    "test/examples/get-with-cookies.json"
    $ mkTestOpenApi Fixtures.getWithCookiesOpenApiRouter "get-with-cookies"

test_openApiSubset :: Tasty.TestTree
test_openApiSubset =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a subset of routes"
    "test/examples/open-api-subset.json"
    $ mkTestOpenApi Fixtures.openApiSubsetRouter "open-api-subset"

test_nullableRefOpenApi :: Tasty.TestTree
test_nullableRefOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a nullable schema"
    "test/examples/nullable-ref.json"
    $ mkTestOpenApi Fixtures.nullableRefOpenApiRouter "nullable-ref"

test_unionOpenApi :: Tasty.TestTree
test_unionOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a union schema"
    "test/examples/union.json"
    $ mkTestOpenApi Fixtures.unionOpenApiRouter "union"

test_taggedUnionOpenApi :: Tasty.TestTree
test_taggedUnionOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a tagged union schema"
    "test/examples/tagged-union.json"
    $ mkTestOpenApi Fixtures.taggedUnionOpenApiRouter "tagged-union"

test_nullableRefCollectComponentsOpenApi :: Tasty.TestTree
test_nullableRefCollectComponentsOpenApi =
  mkGoldenTest
    "Generates the correct OpenAPI JSON for a nullable schema with an inner object schema"
    "test/examples/nullable-ref-collect-components.json"
    $ mkTestOpenApi Fixtures.nullableRefCollectComponentsOpenApiRouter "nullable-ref-collect-components"

mkTestOpenApi :: Orb.OpenApiRouter a -> String -> Either [Orb.OpenApiError] OpenApi.OpenApi
mkTestOpenApi =
  Orb.mkOpenApi Orb.defaultOpenApiOptions

mkGoldenTest ::
  Tasty.TestName ->
  FilePath ->
  Either [Orb.OpenApiError] OpenApi.OpenApi ->
  Tasty.TestTree
mkGoldenTest testName goldenPath errOrOpenApi = do
  -- Using VsStringDiff instead of VsString because the output for failing
  -- tests is better
  goldenVsStringDiff testName (\ref new -> ["diff", "-u", ref, new]) goldenPath $ do
    openApi <-
      either
        (fail . unlines . fmap Orb.renderOpenApiError)
        pure
        errOrOpenApi
    -- Aeson Pretty doesn't emit a newline at the end, but some text editors
    -- like to add it. So we explicitly add it.
    pure $ AesonPretty.encodePretty openApi <> LBS.pack [10]
