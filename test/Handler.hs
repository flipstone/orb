{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( testGroup
  ) where

import Beeline.Routing qualified as R
import Control.Monad.IO.Class qualified as MIO
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
