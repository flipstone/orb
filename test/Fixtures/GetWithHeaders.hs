{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithHeaders
  ( GetWithHeaders (..)
  , TestHeaders (..)
  , getWithHeadersOpenApiRouter
  ) where

import Beeline.Params ((?+))
import Beeline.Params qualified as BP
import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Shrubbery qualified as S

import Fixtures.NoPermissions as Export
import Orb qualified
import TestDispatchM qualified as TDM

getWithHeadersOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[GetWithHeaders])
getWithHeadersOpenApiRouter =
  Orb.provideOpenApi "get-with-headers"
    . R.routeList
    $ Orb.get (R.make GetWithHeaders /- "test" /- "get_with_headers")
      /: R.emptyRoutes

data GetWithHeaders = GetWithHeaders

newtype TestHeaders = TestHeaders
  { headerParam :: T.Text
  }

testHeadersSchema :: BP.ParameterSchema schema => schema TestHeaders TestHeaders
testHeadersSchema =
  BP.makeParams TestHeaders
    ?+ BP.required headerParam (BP.textParam "headerParam")

instance Orb.HasHandler GetWithHeaders where
  type HandlerRequestHeaders GetWithHeaders = TestHeaders
  type HandlerResponses GetWithHeaders = TestResponses
  type HandlerPermissionAction GetWithHeaders = NoPermissions
  type HandlerMonad GetWithHeaders = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response400 Orb.BadRequestMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithHeaders
handler =
  Orb.Handler
    { Orb.handlerId = "getWithHeaders"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.RequestHeaders testHeadersSchema
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
            q = headerParam . Orb.reqHeaders $ request
          in
            Orb.return200 (Orb.SuccessMessage q)
    }
