{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.SimpleGet
  ( SimpleGet (..)
  , simpleGetOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

simpleGetOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[SimpleGet])
simpleGetOpenApiRouter =
  Orb.provideOpenApi "simple-get"
    . R.routeList
    $ Orb.get (R.make SimpleGet /- "test" /- "simple_get")
      /: R.emptyRoutes

data SimpleGet = SimpleGet

instance Orb.HasHandler SimpleGet where
  type HandlerResponses SimpleGet = Responses
  type HandlerPermissionAction SimpleGet = NoPermissions
  type HandlerMonad SimpleGet = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler SimpleGet
handler =
  Orb.Handler
    { Orb.handlerId = "simpleGet"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.EmptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \_request () -> Orb.return200 (Orb.SuccessMessage "simpleGet")
    }
