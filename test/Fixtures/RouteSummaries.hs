{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.RouteSummaries
  ( routeSummariesRouter
  ) where

import Beeline.Routing ((/+), (/-), (/:), (/>))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Fixtures.SimpleGet (SimpleGet (SimpleGet))
import Fixtures.SimplePost (SimplePost (SimplePost))
import Orb qualified
import TestDispatchM qualified as TDM

routeSummariesRouter ::
  Orb.OpenApiProvider r =>
  r
    ( S.Union
        '[ Wrapped ()
         , SimpleGet
         , SimplePost
         , Nested
         , Wrapped (Maybe SimpleGet)
         , BeelineRoute
         ]
    )
routeSummariesRouter =
  R.routeList $
    Orb.get (R.make Wrapped)
      /: Orb.get (R.make SimpleGet /- "simple")
      /: Orb.provideOpenApi "subset" (Orb.post (R.make SimplePost /- "simple"))
      /: ( R.make Nested
             /- "nested"
             /+ R.Param (R.textParam "nestedId") nestedId
             /> R.Subrouter nestedRouter nestedRoute
         )
      /: Orb.get (R.make Wrapped /- "wrapped")
      /: R.get (R.make BeelineRoute /- "beeline")
      /: R.emptyRoutes

nestedRouter :: Orb.OpenApiProvider r => r (S.Union '[SimpleGet])
nestedRouter =
  Orb.provideOpenApi "nested"
    . R.routeList
    $ Orb.get (R.make SimpleGet /- "inner")
      /: R.emptyRoutes

data Nested = Nested
  { nestedId :: T.Text
  , nestedRoute :: S.Union '[SimpleGet]
  }

data BeelineRoute = BeelineRoute

data Wrapped a = Wrapped

instance Orb.HasHandler (Wrapped a) where
  type HandlerResponses (Wrapped a) = WrappedResponses
  type HandlerPermissionAction (Wrapped a) = NoPermissions
  type HandlerMonad (Wrapped a) = TDM.TestDispatchM
  routeHandler = wrappedHandler

type WrappedResponses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

wrappedHandler :: Orb.Handler (Wrapped a)
wrappedHandler =
  Orb.Handler
    { Orb.handlerId = "wrapped"
    , Orb.requestBody = Orb.emptyRequestBody
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \_request () -> Orb.returnResponse Orb.status200 (Orb.SuccessMessage "wrapped")
    }
