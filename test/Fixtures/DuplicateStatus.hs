{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.DuplicateStatus
  ( DuplicateStatus (..)
  , duplicateStatusOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Network.HTTP.Types qualified as HTTP
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

duplicateStatusOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[DuplicateStatus])
duplicateStatusOpenApiRouter =
  Orb.provideOpenApi "duplicate-status"
    . R.routeList
    $ Orb.get (R.make DuplicateStatus /- "test" /- "duplicate_status")
      /: R.emptyRoutes

data DuplicateStatus = DuplicateStatus

alternateOk :: Orb.StatusCode "200-alternate"
alternateOk =
  Orb.mkStatusCode HTTP.status200

instance Orb.HasHandler DuplicateStatus where
  type HandlerResponses DuplicateStatus = Responses
  type HandlerPermissionAction DuplicateStatus = NoPermissions
  type HandlerMonad DuplicateStatus = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "200-alternate" Orb.SuccessMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

handler :: Orb.Handler DuplicateStatus
handler =
  Orb.Handler
    { Orb.handlerId = "duplicateStatus"
    , Orb.requestBody = Orb.emptyRequestBody
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema alternateOk Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \_request () -> Orb.returnResponse Orb.status200 (Orb.SuccessMessage "duplicateStatus")
    }
