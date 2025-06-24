{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SwaggerUI
  ( testGroup
  ) where

import Beeline.Routing qualified as R
import Control.Monad.IO.Class qualified as MIO
import Control.Monad.Reader qualified as Reader
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.FileEmbed qualified as FileEmbed
import Hedgehog ((===))
import Hedgehog qualified as HH
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Fixtures qualified
import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "SwaggerUI"
    [ TastyHH.testProperty "serves swagger ui index" prop_swaggerUIIndex
    , TastyHH.testProperty "redirects to index with /" prop_swaggerUIIndexRedirect
    , TastyHH.testProperty "renders OpenApi JSON" prop_swaggerUIOpenApi
    , TastyHH.testProperty "serves swagger ui resources" prop_swaggerUIResource
    ]

prop_swaggerUIIndex :: HH.Property
prop_swaggerUIIndex = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath (Wai.defaultRequest) "/basic-open-api/"

  HH.evalIO . WaiTest.withSession swaggerUIApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBodyContains "<!DOCTYPE html>" response

prop_swaggerUIIndexRedirect :: HH.Property
prop_swaggerUIIndexRedirect = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath (Wai.defaultRequest) "/basic-open-api"

  HH.evalIO . WaiTest.withSession swaggerUIApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 308 response
    WaiTest.assertHeader "Location" "/basic-open-api/" response

prop_swaggerUIOpenApi :: HH.Property
prop_swaggerUIOpenApi = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath (Wai.defaultRequest) "/basic-open-api/open-api.json"

  response <- HH.evalIO . WaiTest.withSession swaggerUIApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    pure response

  LBS8.lines (WaiTest.simpleBody response)
    === LBS8.lines (LBS.fromStrict $(FileEmbed.embedFile "test/examples/basic-open-api.json"))

prop_swaggerUIResource :: HH.Property
prop_swaggerUIResource = HH.withTests 1 . HH.property $ do
  let
    request =
      WaiTest.setPath (Wai.defaultRequest) "/basic-open-api/index.css"

  HH.evalIO . WaiTest.withSession swaggerUIApp $ do
    response <- WaiTest.request request
    WaiTest.assertStatus 200 response
    WaiTest.assertBodyContains "html {" response

swaggerUIRouter :: R.Router r => r Orb.SwaggerUIRoute
swaggerUIRouter =
  Orb.swaggerUIRoutes Fixtures.basicOpenApiRouter

swaggerUIApp :: Wai.Application
swaggerUIApp =
  Orb.orbAppToWai $
    Orb.OrbApp
      { Orb.router = swaggerUIRouter
      , Orb.dispatcher = runTestDispatchM . Orb.dispatch
      , Orb.handleNotFound = Orb.defaultHandleNotFound
      }

data TestDispatchEnv = TestDispatchEnv
  { testDispatchRequest :: Wai.Request
  , testDispatchRespond :: Wai.Response -> IO Wai.ResponseReceived
  }

newtype TestDispatchM a
  = TestDispatchM (Reader.ReaderT TestDispatchEnv IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    )

instance Orb.HasRequest TestDispatchM where
  request = TestDispatchM (Reader.asks testDispatchRequest)

instance Orb.HasRespond TestDispatchM where
  respond = TestDispatchM (Reader.asks testDispatchRespond)

runTestDispatchM :: TestDispatchM Wai.ResponseReceived -> Wai.Application
runTestDispatchM (TestDispatchM reader) request respond =
  let
    env =
      TestDispatchEnv
        { testDispatchRequest = request
        , testDispatchRespond = respond
        }
  in
    Reader.runReaderT reader env
