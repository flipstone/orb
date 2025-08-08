{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithQuery
  ( GetWithQuery (..)
  , TestQuery (..)
  , getWithQueryOpenApiRouter
  ) where

import Beeline.Params ((?+))
import Beeline.Params qualified as BP
import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Int qualified as Int
import Data.Text qualified as T
import Shrubbery qualified as S

import Fixtures.NoPermissions as Export
import Orb qualified
import TestDispatchM qualified as TDM

getWithQueryOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[GetWithQuery])
getWithQueryOpenApiRouter =
  Orb.provideOpenApi "get-with-query"
    . R.routeList
    $ Orb.get (R.make GetWithQuery /- "test" /- "get_with_query")
      /: R.emptyRoutes

data GetWithQuery = GetWithQuery

data TestQuery = TestQuery
  { textQueryParam :: T.Text
  , int8QueryParam :: Maybe Int.Int8
  }

testQuerySchema :: BP.QuerySchema schema => schema TestQuery TestQuery
testQuerySchema =
  BP.makeParams TestQuery
    ?+ BP.required textQueryParam (BP.textParam "textQueryParam")
    ?+ BP.optional int8QueryParam (BP.int8Param "int8QueryParam")

instance Orb.HasHandler GetWithQuery where
  type HandlerRequestQuery GetWithQuery = TestQuery
  type HandlerResponses GetWithQuery = TestResponses
  type HandlerPermissionAction GetWithQuery = NoPermissions
  type HandlerMonad GetWithQuery = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response400 Orb.BadRequestMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithQuery
handler =
  Orb.Handler
    { Orb.handlerId = "getWithQuery"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.RequestQuery testQuerySchema
    , Orb.requestHeaders = Orb.EmptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema400 Orb.badRequestMessageSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \request () ->
          let
            q = textQueryParam . Orb.reqQuery $ request
          in
            Orb.return200 (Orb.SuccessMessage q)
    }
