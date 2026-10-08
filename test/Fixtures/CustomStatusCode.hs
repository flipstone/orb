{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.CustomStatusCode
  ( CustomStatusCode (..)
  , customStatusCodeOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Network.HTTP.Types qualified as HTTP
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

customStatusCodeOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[CustomStatusCode])
customStatusCodeOpenApiRouter =
  Orb.provideOpenApi "custom-status-code"
    . R.routeList
    $ Orb.get (R.make CustomStatusCode /- "test" /- "custom_status_code")
      /: R.emptyRoutes

data CustomStatusCode = CustomStatusCode

status499 :: Orb.StatusCode "499"
status499 =
  Orb.mkStatusCode (HTTP.mkStatus 499 "Client Closed Request")

instance Orb.HasHandler CustomStatusCode where
  type HandlerResponses CustomStatusCode = Responses
  type HandlerPermissionAction CustomStatusCode = NoPermissions
  type HandlerMonad CustomStatusCode = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "499" Orb.SuccessMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

handler :: Orb.Handler CustomStatusCode
handler =
  Orb.Handler
    { Orb.handlerId = "customStatusCode"
    , Orb.requestBody = Orb.emptyRequestBody
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema status499 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \_request () -> Orb.returnResponse status499 (Orb.SuccessMessage "customStatusCode")
    }
