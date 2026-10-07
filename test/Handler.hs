{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( testGroup
  ) where

import Beeline.Routing qualified as R
import Control.Monad.IO.Class qualified as MIO
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Hedgehog qualified as HH
import Network.HTTP.Types qualified as HTTPTypes
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Fixtures qualified
import Orb qualified
import TestDispatchM qualified as TDM

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "Handler"
    [ TastyHH.testProperty "serves a simple get" prop_simpleGet
    , TastyHH.testProperty "serves a simple post" prop_simplePost
    , TastyHH.testProperty "serves a get with a query" prop_getWithQuery
    , TastyHH.testProperty "responds to invalid query params with error" prop_getWithQueryError
    , TastyHH.testProperty "serves a get with a header param" prop_getWithHeaders
    , TastyHH.testProperty "responds to invalid header params with error" prop_getWithHeadersError
    , TastyHH.testProperty "serves a get with a cookie param" prop_getWithCookies
    , TastyHH.testProperty "responds to invalid cookie params with error" prop_getWithCookiesError
    , TastyHH.testProperty "serves a custom status code" prop_customStatusCode
    , TastyHH.testProperty "serves a schema body with a custom parse error response" prop_customBodyError
    , TastyHH.testProperty "responds to an unparseable schema body with a custom error" prop_customBodyErrorInvalid
    , TastyHH.testProperty "decodes a deferred body once in the handler" prop_deferredBody
    , TastyHH.testProperty "lets the handler answer a deferred body decode failure" prop_deferredBodyInvalid
    , TastyHH.testProperty "checks permission before reading a deferred body" prop_deferredBodyUnauthorized
    , TastyHH.testProperty "checks permission before decoding a schema body" prop_permissionBeforeBody
    , TastyHH.testProperty "reuses a body the permission action decoded" prop_permissionReadsBody
    , TastyHH.testProperty "lets a permission action reject based on the body" prop_permissionRejectsBody
    , TastyHH.testProperty "answers a body decode failure after permission" prop_permissionReadsInvalidBody
    ]

prop_simpleGet :: HH.Property
prop_simpleGet = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/simple_get"

  evalAppSession Fixtures.simpleGetOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"simpleGet\"}" response

prop_simplePost :: HH.Property
prop_simplePost = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/simple_post")
        { Wai.requestMethod = HTTPTypes.methodPost
        }

  evalAppSession Fixtures.simplePostOpenApiRouter $ do
    response <- WaiTest.srequest (WaiTest.SRequest request "{\"postParam\": \"value\"}")
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"value\"}" response

prop_getWithQuery :: HH.Property
prop_getWithQuery = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/get_with_query?textQueryParam=queryValue"

  evalAppSession Fixtures.getWithQueryOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"queryValue\"}" response

prop_getWithQueryError :: HH.Property
prop_getWithQueryError = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/get_with_query?wrongParam=queryValue"

  evalAppSession Fixtures.getWithQueryOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 400 response
    WaiTest.assertBody "{\"bad_request\":\"Required query param missing: textQueryParam\"}" response

prop_getWithHeaders :: HH.Property
prop_getWithHeaders = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/get_with_headers")
        { Wai.requestHeaders = [("headerParam", "headerValue")]
        }

  evalAppSession Fixtures.getWithHeadersOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"headerValue\"}" response

prop_getWithHeadersError :: HH.Property
prop_getWithHeadersError = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/get_with_headers")
        { Wai.requestHeaders = [("wrongParam", "headerValue")]
        }

  evalAppSession Fixtures.getWithHeadersOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 400 response
    WaiTest.assertBody "{\"bad_request\":\"Required header param missing: headerParam\"}" response

prop_getWithCookies :: HH.Property
prop_getWithCookies = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/get_with_cookies")
        { Wai.requestHeaders = [("Cookie", "cookieParam=cookieValue")]
        }

  evalAppSession Fixtures.getWithCookiesOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"cookieValue\"}" response

prop_getWithCookiesError :: HH.Property
prop_getWithCookiesError = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/get_with_cookies")
        { Wai.requestHeaders = [("Cookie", "wrongParam=cookieValue")]
        }

  evalAppSession Fixtures.getWithCookiesOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 400 response
    WaiTest.assertBody "{\"bad_request\":\"Required cookie param missing: cookieParam\"}" response

prop_customStatusCode :: HH.Property
prop_customStatusCode = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/custom_status_code"

  evalAppSession Fixtures.customStatusCodeOpenApiRouter $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 499 response
    WaiTest.assertBody "{\"success\":\"customStatusCode\"}" response

prop_customBodyError :: HH.Property
prop_customBodyError = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/custom_body_error")
        { Wai.requestMethod = HTTPTypes.methodPost
        }

  evalAppSession Fixtures.customBodyErrorOpenApiRouter $ do
    response <- WaiTest.srequest (WaiTest.SRequest request "{\"postParam\": \"value\"}")
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"value\"}" response

prop_customBodyErrorInvalid :: HH.Property
prop_customBodyErrorInvalid = HH.withTests 1 . HH.property $ do
  let
    request =
      (WaiTest.setPath Wai.defaultRequest "/test/custom_body_error")
        { Wai.requestMethod = HTTPTypes.methodPost
        }

  evalAppSession Fixtures.customBodyErrorOpenApiRouter $ do
    response <- WaiTest.srequest (WaiTest.SRequest request "{\"wrongParam\": \"value\"}")
    WaiTest.assertStatus 400 response
    WaiTest.assertContentType "application/json" response
    WaiTest.assertBodyContains "\"errorCode\":\"invalid_body\"" response

prop_deferredBody :: HH.Property
prop_deferredBody = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.deferredBodyOpenApiRouter $ do
    response <- WaiTest.srequest (deferredBodyRequest "Bearer good" "{\"postParam\": \"value\"}")
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"value,value\"}" response

prop_deferredBodyInvalid :: HH.Property
prop_deferredBodyInvalid = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.deferredBodyOpenApiRouter $ do
    response <- WaiTest.srequest (deferredBodyRequest "Bearer good" "{\"wrongParam\": \"value\"}")
    WaiTest.assertStatus 400 response
    WaiTest.assertContentType "application/json" response

prop_deferredBodyUnauthorized :: HH.Property
prop_deferredBodyUnauthorized = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.deferredBodyOpenApiRouter $ do
    response <- WaiTest.srequest (deferredBodyRequest "Bearer bad" "not json")
    WaiTest.assertStatus 401 response
    WaiTest.assertBody "{\"unauthorized\":\"Invalid token\"}" response

deferredBodyRequest :: BS.ByteString -> LBS.ByteString -> WaiTest.SRequest
deferredBodyRequest token =
  WaiTest.SRequest
    (WaiTest.setPath Wai.defaultRequest "/test/deferred_body")
      { Wai.requestMethod = HTTPTypes.methodPost
      , Wai.requestHeaders = [(HTTPTypes.hAuthorization, token)]
      }

prop_permissionBeforeBody :: HH.Property
prop_permissionBeforeBody = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.permissionReadsBodyOpenApiRouter $ do
    response <- WaiTest.srequest (permissionReadsBodyRequest "Bearer bad" "not json")
    WaiTest.assertStatus 401 response
    WaiTest.assertBody "{\"unauthorized\":\"Invalid token\"}" response

prop_permissionReadsBody :: HH.Property
prop_permissionReadsBody = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.permissionReadsBodyOpenApiRouter $ do
    response <- WaiTest.srequest (permissionReadsBodyRequest "Bearer good" "{\"postParam\": \"allowed\"}")
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"allowed\"}" response

prop_permissionRejectsBody :: HH.Property
prop_permissionRejectsBody = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.permissionReadsBodyOpenApiRouter $ do
    response <- WaiTest.srequest (permissionReadsBodyRequest "Bearer good" "{\"postParam\": \"denied\"}")
    WaiTest.assertStatus 401 response
    WaiTest.assertBody "{\"unauthorized\":\"Not allowed\"}" response

prop_permissionReadsInvalidBody :: HH.Property
prop_permissionReadsInvalidBody = HH.withTests 1 . HH.property $
  evalAppSession Fixtures.permissionReadsBodyOpenApiRouter $ do
    response <- WaiTest.srequest (permissionReadsBodyRequest "Bearer good" "not json")
    WaiTest.assertStatus 422 response

permissionReadsBodyRequest :: BS.ByteString -> LBS.ByteString -> WaiTest.SRequest
permissionReadsBodyRequest token =
  WaiTest.SRequest
    (WaiTest.setPath Wai.defaultRequest "/test/permission_reads_body")
      { Wai.requestMethod = HTTPTypes.methodPost
      , Wai.requestHeaders = [(HTTPTypes.hAuthorization, token)]
      }

evalAppSession ::
  ( Orb.Dispatchable TDM.TestDispatchM a
  , HH.MonadTest m
  , MIO.MonadIO m
  ) =>
  R.RouteRecognizer a ->
  WaiTest.Session () ->
  m ()
evalAppSession recognizer testSession =
  HH.evalIO . WaiTest.withSession (testApp recognizer) $ testSession

testApp ::
  Orb.Dispatchable TDM.TestDispatchM a =>
  R.RouteRecognizer a ->
  Wai.Application
testApp router =
  Orb.orbAppToWai $
    Orb.OrbApp
      { Orb.router = router
      , Orb.dispatcher = TDM.runTestDispatchM . Orb.dispatch
      , Orb.handleNotFound = Orb.defaultHandleNotFound
      }
