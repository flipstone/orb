{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fixtures.PermissionReadsBody
  ( PermissionReadsBody (..)
  , permissionReadsBodyOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Shrubbery qualified as S

import Orb qualified
import TestDispatchM qualified as TDM

permissionReadsBodyOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[PermissionReadsBody])
permissionReadsBodyOpenApiRouter =
  Orb.provideOpenApi "permission-reads-body"
    . R.routeList
    $ Orb.post (R.make PermissionReadsBody /- "test" /- "permission_reads_body")
      /: R.emptyRoutes

data PermissionReadsBody = PermissionReadsBody

instance Orb.HasHandler PermissionReadsBody where
  type HandlerRequestBody PermissionReadsBody = PermissionReadsBodyBody
  type HandlerResponses PermissionReadsBody = Responses
  type HandlerPermissionAction PermissionReadsBody = BodyPermission
  type HandlerMonad PermissionReadsBody = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "401" Orb.UnauthorizedMessage
  , Orb.Response "422" Orb.UnprocessableContentMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

newtype PermissionReadsBodyBody = PermissionReadsBodyBody
  { permissionReadsBodyBodyParam :: T.Text
  }

permissionReadsBodyBodySchema :: FC.Fleece t => FC.Schema t PermissionReadsBodyBody
permissionReadsBodyBodySchema =
  FC.object $
    FC.constructor PermissionReadsBodyBody
      #+ FC.required "postParam" permissionReadsBodyBodyParam FC.text

newtype BodyPermission
  = BodyPermission (Orb.BodyHandle (S.TaggedUnion Responses) PermissionReadsBodyBody)

newtype BodyPermissionError = BodyPermissionError T.Text

instance Orb.PermissionAction BodyPermission where
  type PermissionActionMonad BodyPermission = TDM.TestDispatchM
  type PermissionActionError BodyPermission = BodyPermissionError
  type PermissionActionResult BodyPermission = ()

  checkPermissionAction (BodyPermission body) = do
    request <- Orb.request
    if lookup HTTP.hAuthorization (Wai.requestHeaders request) /= Just "Bearer good"
      then pure (Left (BodyPermissionError "Invalid token"))
      else do
        errOrBody <- Orb.decodeBody body
        pure $
          case errOrBody of
            Right decoded
              | permissionReadsBodyBodyParam decoded /= "allowed" ->
                  Left (BodyPermissionError "Not allowed")
            _ ->
              Right ()

instance Orb.PermissionError BodyPermissionError where
  type PermissionErrorConstraints BodyPermissionError tags = Orb.Has401Response tags
  type PermissionErrorMonad BodyPermissionError = TDM.TestDispatchM

  returnPermissionError (BodyPermissionError message) =
    Orb.returnResponse Orb.status401 (Orb.UnauthorizedMessage message)

handler :: Orb.Handler PermissionReadsBody
handler =
  Orb.Handler
    { Orb.handlerId = "permissionReadsBody"
    , Orb.requestBody = Orb.schemaRequestBody permissionReadsBodyBodySchema
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status401 Orb.unauthorizedMessageSchema
          . Orb.addResponseSchema Orb.status422 Orb.unprocessableContentSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        BodyPermission . Orb.permissionRequestBody
    , Orb.handleRequest =
        \request () ->
          Orb.returnResponse Orb.status200
            . Orb.SuccessMessage
            . permissionReadsBodyBodyParam
            . Orb.reqBody
            $ request
    }
