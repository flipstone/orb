{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy as LBS
import Data.FileEmbed qualified as FileEmbed
import Data.OpenApi qualified as OpenApi
import Data.Void qualified as Void
import Hedgehog ((===))
import Hedgehog qualified as HH
import Shrubbery qualified as S
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Orb qualified

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Orb"
      [ testGroup
      ]

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
  openApi <- HH.evalEither (Orb.mkOpenApi router "basic-open-api")
  assertEqualOpenApi openApi $(FileEmbed.embedFile "test/examples/basic-open-api.json")

prop_openApiUnknownLabel :: HH.Property
prop_openApiUnknownLabel = HH.withTests 1 . HH.property $ do
  case Orb.mkOpenApi router "unknown-open-api" of
    Right _ -> fail "Should not have returned an OpenApi for an unknown label"
    Left msg -> msg === "No OpenApi definition found with label unknown-open-api."

prop_openApiSubset :: HH.Property
prop_openApiSubset = HH.withTests 1 . HH.property $ do
  openApi <- HH.evalEither (Orb.mkOpenApi router "just-route-1")
  assertEqualOpenApi openApi $(FileEmbed.embedFile "test/examples/just-route-1.json")

router :: Orb.OpenApiProvider r => r (S.Union [TestRoute1, TestRoute2])
router =
  Orb.provideOpenApi "basic-open-api"
    . R.routeList
    $ Orb.provideOpenApi "just-route-1" (Orb.get (R.make TestRoute1 /- "test/route1"))
      /: Orb.get (R.make TestRoute2 /- "test/route2")
      /: R.emptyRoutes

assertEqualOpenApi :: HH.MonadTest m => OpenApi.OpenApi -> BS8.ByteString -> m ()
assertEqualOpenApi openApi expectedBytes = do
  -- Splitting on lines here both produces a more useful diff and allows for small
  -- meaningless differences such as newline at end of file between the encoded
  -- bytes and the expected bytes read from the sample file
  BS8.lines (LBS.toStrict (AesonPretty.encodePretty openApi))
    === BS8.lines expectedBytes

data TestRoute1 = TestRoute1

instance Orb.HasHandler TestRoute1 where
  type HandlerRequestBody TestRoute1 = Orb.NoRequestBody
  type HandlerResponses TestRoute1 = TestResponses
  type HandlerPermissionAction TestRoute1 = NoPermissions
  type HandlerMonad TestRoute1 = IO
  routeHandler = mkTestHandler "testRoute1"

data TestRoute2 = TestRoute2

instance Orb.HasHandler TestRoute2 where
  type HandlerRequestBody TestRoute2 = Orb.NoRequestBody
  type HandlerResponses TestRoute2 = TestResponses
  type HandlerPermissionAction TestRoute2 = NoPermissions
  type HandlerMonad TestRoute2 = IO
  routeHandler = mkTestHandler "testRoute2"

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response500 Orb.InternalServerError
  ]

mkTestHandler ::
  ( Orb.HandlerPermissionAction route ~ NoPermissions
  , Orb.HandlerResponses route ~ TestResponses
  , Orb.HandlerRequestBody route ~ Orb.NoRequestBody
  , Applicative (Orb.HandlerMonad route)
  ) =>
  String ->
  Orb.Handler route
mkTestHandler handlerId =
  Orb.Handler
    { Orb.handlerId = handlerId
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_route _request -> NoPermissions
    , Orb.handleRequest =
        \_route Orb.NoRequestBody () ->
          Orb.return200 (Orb.SuccessMessage "Hi")
    }

data NoPermissions
  = NoPermissions

newtype NoError = NoError Void.Void

instance Orb.PermissionAction NoPermissions where
  type PermissionActionMonad NoPermissions = IO
  type PermissionActionError NoPermissions = NoError
  type PermissionActionResult NoPermissions = ()

  checkPermissionAction _ =
    pure (Right ())

instance Orb.PermissionError NoError where
  type PermissionErrorConstraints NoError _tags = ()
  type PermissionErrorMonad NoError = IO

  returnPermissionError (NoError void) =
    Void.absurd void
