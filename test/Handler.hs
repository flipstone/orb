{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
  ( testGroup
  ) where

import Hedgehog qualified as HH
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
    , TastyHH.testProperty "serves a get with a query" prop_getWithQuery
    , TastyHH.testProperty "responds to invalid query params with error" prop_getWithQueryError
    , TastyHH.testProperty "serves a get with a header param" prop_getWithHeaders
    , TastyHH.testProperty "responds to invalid header params with error" prop_getWithHeadersError
    ]

prop_simpleGet :: HH.Property
prop_simpleGet = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/simple_get"

  HH.evalIO . WaiTest.withSession basicApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"simpleGet\"}" response

prop_getWithQuery :: HH.Property
prop_getWithQuery = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/get_with_query?textQueryParam=queryValue"

  HH.evalIO . WaiTest.withSession basicApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBody "{\"success\":\"queryValue\"}" response

prop_getWithQueryError :: HH.Property
prop_getWithQueryError = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath Wai.defaultRequest "/test/get_with_query?wrongParam=queryValue"

  HH.evalIO . WaiTest.withSession basicApp $ do
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

  HH.evalIO . WaiTest.withSession basicApp $ do
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

  HH.evalIO . WaiTest.withSession basicApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 400 response
    WaiTest.assertBody "{\"bad_request\":\"Required header param missing: headerParam\"}" response

basicApp :: Wai.Application
basicApp =
  Orb.orbAppToWai $
    Orb.OrbApp
      { Orb.router = Fixtures.basicOpenApiRouter
      , Orb.dispatcher = TDM.runTestDispatchM . Orb.dispatch
      , Orb.handleNotFound = Orb.defaultHandleNotFound
      }
