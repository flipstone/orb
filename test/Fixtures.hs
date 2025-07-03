{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fixtures
  ( TestRoute1 (TestRoute1)
  , TestRoute2 (TestRoute2)
  , mkTestHandler
  , TestResponses
  , basicOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Data.Void qualified as Void
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Orb qualified

basicOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union [TestRoute1, TestRoute2])
basicOpenApiRouter =
  Orb.provideOpenApi "basic-open-api"
    . R.routeList
    $ Orb.provideOpenApi "just-route-1" (Orb.get (R.make TestRoute1 /- "test/route1"))
      /: Orb.get (R.make TestRoute2 /- "test/route2")
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
  , Orb.Response400 (Either Int RandomObject)
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
          . Orb.addResponseSchema400 intOrObjectSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_route _request -> NoPermissions
    , Orb.handleRequest =
        \_route Orb.NoRequestBody () ->
          Orb.return200 (Orb.SuccessMessage "Hi")
    }

intOrObjectSchema :: FC.Fleece schema => schema (Either Int RandomObject)
intOrObjectSchema =
  FC.eitherOfNamed "IntOrObject" FC.int randomObjectSchema

data RandomObject = RandomObject
  { randomBool :: Bool
  , randomText :: T.Text
  }

randomObjectSchema :: FC.Fleece schema => schema RandomObject
randomObjectSchema =
  FC.object $
    FC.constructor RandomObject
      #+ FC.required "bool" randomBool FC.boolean
      #+ FC.required "text" randomText FC.text

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
