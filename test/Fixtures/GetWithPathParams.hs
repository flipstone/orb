{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithPathParams
  ( GetWithPathParams (..)
  , getWithPathParamsOpenApiRouter
  ) where

import Beeline.Routing ((/+), (/-), (/:))
import Beeline.Routing qualified as R
import Data.Int qualified as Int
import Data.Text qualified as T
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

getWithPathParamsOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[GetWithPathParams])
getWithPathParamsOpenApiRouter =
  Orb.provideOpenApi "get-with-path-params"
    . R.routeList
    $ Orb.get
      ( R.make GetWithPathParams
          /- "test"
          /+ R.Param (R.textParam "textPathParam") getWithPathParamsTextParam
          /+ R.Param (R.int32Param "int32PathParam") getWithPathParamsInt32Param
      )
      /: R.emptyRoutes

data GetWithPathParams = GetWithPathParams
  { getWithPathParamsTextParam :: T.Text
  , getWithPathParamsInt32Param :: Int.Int32
  }

instance Orb.HasHandler GetWithPathParams where
  type HandlerResponses GetWithPathParams = TestResponses
  type HandlerPermissionAction GetWithPathParams = NoPermissions
  type HandlerMonad GetWithPathParams = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithPathParams
handler =
  Orb.Handler
    { Orb.handlerId = "getWithPathParams"
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
        \_request () ->
          Orb.return200 (Orb.SuccessMessage "getWithPathParams")
    }
