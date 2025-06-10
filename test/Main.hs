{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main
  ( main
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Void qualified as Void
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
    "BoundedText"
    [ TastyHH.testProperty "can build text within bounds" prop_openApi
    ]

prop_openApi :: HH.Property
prop_openApi = HH.withTests 1 . HH.property $ do
  pure ()
  -- let
  --   zeroChar :: Either BoundedText.BoundedTextError (BoundedText.BoundedText 1 0)
  --   zeroChar = BoundedText.boundedTextFromText ""

  -- zeroChar === Left BoundedText.TextLengthBelowMinimum

router :: Orb.ServerRouter r => r (S.Union [TestRoute1, TestRoute2])
router =
  R.routeList $
    Orb.get (R.make TestRoute1 /- "/test/route1")
      /: Orb.get (R.make TestRoute2 /- "/test/route2")
      /: R.emptyRoutes

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

data NoPermissions =
  NoPermissions
  
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


