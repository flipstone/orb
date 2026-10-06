{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.SimplePost
  ( SimplePost (..)
  , simplePostOpenApiRouter
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

simplePostOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[SimplePost])
simplePostOpenApiRouter =
  Orb.provideOpenApi "simple-post"
    . R.routeList
    $ Orb.post (R.make SimplePost /- "test" /- "simple_post")
      /: R.emptyRoutes

data SimplePost = SimplePost

instance Orb.HasHandler SimplePost where
  type HandlerRequestBody SimplePost = SimplePostBody
  type HandlerResponses SimplePost = Responses
  type HandlerPermissionAction SimplePost = NoPermissions
  type HandlerMonad SimplePost = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response "200" Orb.SuccessMessage
  , Orb.Response "422" Orb.UnprocessableContentMessage
  , Orb.Response "500" Orb.InternalServerError
  ]

newtype SimplePostBody = SimplePostBody
  { simplePostParam :: T.Text
  }

simplePostBodySchema :: FC.Fleece t => FC.Schema t SimplePostBody
simplePostBodySchema =
  FC.object $
    FC.constructor SimplePostBody
      #+ FC.required "postParam" simplePostParam FC.text

handler :: Orb.Handler SimplePost
handler =
  Orb.Handler
    { Orb.handlerId = "simplePost"
    , Orb.requestBody = Orb.schemaRequestBody simplePostBodySchema
    , Orb.requestQuery = Orb.emptyRequestQuery
    , Orb.requestHeaders = Orb.emptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema Orb.status200 Orb.successMessageSchema
          . Orb.addResponseSchema Orb.status422 Orb.unprocessableContentSchema
          . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \request () ->
          Orb.returnResponse Orb.status200
            . Orb.SuccessMessage
            . simplePostParam
            . Orb.reqBody
            $ request
    }
