{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithCookies
  ( GetWithCookies (..)
  , TestCookies (..)
  , getWithCookiesOpenApiRouter
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

getWithCookiesOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[GetWithCookies])
getWithCookiesOpenApiRouter =
  Orb.provideOpenApi "get-with-cookies"
    . R.routeList
    $ Orb.get (R.make GetWithCookies /- "test" /- "get_with_cookies")
      /: R.emptyRoutes

data GetWithCookies = GetWithCookies

newtype TestHeadersWithCookies = TestHeadersWithCookies
  { testCookies :: TestCookies
  }

testHeadersWithCookiesSchema ::
  BP.HeaderSchema schema =>
  schema TestHeadersWithCookies TestHeadersWithCookies
testHeadersWithCookiesSchema =
  BP.makeParams TestHeadersWithCookies
    ?+ BP.cookies testCookies testCookiesSchema

newtype TestCookies = TestCookies
  { cookieParam :: T.Text
  }

testCookiesSchema :: BP.ParameterSchema schema => schema TestCookies TestCookies
testCookiesSchema =
  BP.makeParams TestCookies
    ?+ BP.required cookieParam (BP.textParam "cookieParam")

instance Orb.HasHandler GetWithCookies where
  type HandlerRequestHeaders GetWithCookies = TestHeadersWithCookies
  type HandlerResponses GetWithCookies = TestResponses
  type HandlerPermissionAction GetWithCookies = NoPermissions
  type HandlerMonad GetWithCookies = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response400 Orb.BadRequestMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithCookies
handler =
  Orb.Handler
    { Orb.handlerId = "getWithCookies"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.RequestHeaders testHeadersWithCookiesSchema
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
            q = cookieParam . testCookies . Orb.reqHeaders $ request
          in
            Orb.return200 (Orb.SuccessMessage q)
    }
