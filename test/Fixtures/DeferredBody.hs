{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fixtures.DeferredBody
  ( DeferredBody (..)
  , deferredBodyOpenApiRouter
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

deferredBodyOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[DeferredBody])
deferredBodyOpenApiRouter =
  Orb.provideOpenApi "deferred-body"
    . R.routeList
    $ Orb.post (R.make DeferredBody /- "test" /- "deferred_body")
      /: R.emptyRoutes

data DeferredBody = DeferredBody

instance Orb.HasHandler DeferredBody where
  type HandlerRequestBody DeferredBody = Orb.BodyHandle (S.TaggedUnion Responses) DeferredBodyBody
  type HandlerResponses DeferredBody = Responses
  type HandlerPermissionAction DeferredBody = TokenPermission
  type HandlerMonad DeferredBody = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "400" Orb.BadRequestMessage
  , Orb.Response "401" Orb.UnauthorizedMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

newtype DeferredBodyBody = DeferredBodyBody
  { deferredBodyBodyParam :: T.Text
  }

deferredBodyBodySchema :: FC.Fleece t => FC.Schema t DeferredBodyBody
deferredBodyBodySchema =
  FC.object $
    FC.constructor DeferredBodyBody
      #+ FC.required "postParam" deferredBodyBodyParam FC.text

data TokenPermission = TokenPermission

newtype TokenError = TokenError T.Text

instance Orb.PermissionAction TokenPermission where
  type PermissionActionMonad TokenPermission = TDM.TestDispatchM
  type PermissionActionError TokenPermission = TokenError
  type PermissionActionResult TokenPermission = ()

  checkPermissionAction TokenPermission = do
    request <- Orb.request
    pure $
      if lookup HTTP.hAuthorization (Wai.requestHeaders request) == Just "Bearer good"
        then Right ()
        else Left (TokenError "Invalid token")

instance Orb.PermissionError TokenError where
  type PermissionErrorConstraints TokenError tags = Orb.Has401Response tags
  type PermissionErrorMonad TokenError = TDM.TestDispatchM

  returnPermissionError (TokenError message) =
    Orb.returnResponse Orb.status401 (Orb.UnauthorizedMessage message)

handler :: Orb.Handler DeferredBody
handler =
  Orb.Handler
    { Orb.handlerId = "deferredBody"
    , Orb.requestBody =
        Orb.Deferred
          (Orb.schemaRequestBodyWith (Orb.mkResponse Orb.status400 . Orb.BadRequestMessage) deferredBodyBodySchema)
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status400 Orb.badRequestMessageSchema
          . Orb.addResponseSchema Orb.status401 Orb.unauthorizedMessageSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> TokenPermission
    , Orb.handleRequest =
        \request () -> do
          firstDecode <- Orb.decodeBody (Orb.reqBody request)
          secondDecode <- Orb.decodeBody (Orb.reqBody request)
          case (firstDecode, secondDecode) of
            (Right first, Right second) ->
              Orb.returnResponse Orb.status200
                . Orb.SuccessMessage
                $ deferredBodyBodyParam first <> "," <> deferredBodyBodyParam second
            (Left errResponse, _) ->
              pure errResponse
            (_, Left errResponse) ->
              pure errResponse
    }
