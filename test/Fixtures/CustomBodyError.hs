{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.CustomBodyError
  ( CustomBodyError (..)
  , customBodyErrorOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

customBodyErrorOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[CustomBodyError])
customBodyErrorOpenApiRouter =
  Orb.provideOpenApi "custom-body-error"
    . R.routeList
    $ Orb.post (R.make CustomBodyError /- "test" /- "custom_body_error")
      /: R.emptyRoutes

data CustomBodyError = CustomBodyError

instance Orb.HasHandler CustomBodyError where
  type HandlerRequestBody CustomBodyError = CustomBodyErrorBody
  type HandlerResponses CustomBodyError = Responses
  type HandlerPermissionAction CustomBodyError = NoPermissions
  type HandlerMonad CustomBodyError = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "400" ErrorBody
  , Orb.Response "500" Orb.InternalServerError
  ]

newtype CustomBodyErrorBody = CustomBodyErrorBody
  { customBodyErrorBodyParam :: T.Text
  }

customBodyErrorBodySchema :: FC.Fleece t => FC.Schema t CustomBodyErrorBody
customBodyErrorBodySchema =
  FC.object $
    FC.constructor CustomBodyErrorBody
      #+ FC.required "postParam" customBodyErrorBodyParam FC.text

data ErrorBody = ErrorBody
  { errorBodyCode :: T.Text
  , errorBodyMessage :: T.Text
  }

errorBodySchema :: FC.Fleece t => FC.Schema t ErrorBody
errorBodySchema =
  FC.object $
    FC.constructor ErrorBody
      #+ FC.required "errorCode" errorBodyCode FC.text
      #+ FC.required "errorMessage" errorBodyMessage FC.text

jsonRequestBody ::
  Orb.HasResponseCodeWithType tags "400" ErrorBody =>
  (forall t. FC.Fleece t => FC.Schema t body) ->
  Orb.RequestBody body tags
jsonRequestBody =
  Orb.schemaRequestBodyWith (Orb.mkResponse Orb.status400 . ErrorBody "invalid_body")

handler :: Orb.Handler CustomBodyError
handler =
  Orb.Handler
    { Orb.handlerId = "customBodyError"
    , Orb.requestBody =
        jsonRequestBody customBodyErrorBodySchema
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status400 errorBodySchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \request () ->
          Orb.returnResponse Orb.status200
            . Orb.SuccessMessage
            . customBodyErrorBodyParam
            . Orb.reqBody
            $ request
    }
