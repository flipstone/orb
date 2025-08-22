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
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response422 Orb.UnprocessableContentMessage
  , Orb.Response500 Orb.InternalServerError
  ]

newtype SimplePostBody = SimplePostBody
  { simplePostParam :: T.Text
  }

simplePostBodySchema :: FC.Fleece schema => schema SimplePostBody
simplePostBodySchema =
  FC.object $
    FC.constructor SimplePostBody
      #+ FC.required "postParam" simplePostParam FC.text

handler :: Orb.Handler SimplePost
handler =
  Orb.Handler
    { Orb.handlerId = "simplePost"
    , Orb.requestBody = Orb.SchemaRequestBody simplePostBodySchema
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.EmptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema422 Orb.unprocessableContentSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \request () ->
          Orb.return200
            . Orb.SuccessMessage
            . simplePostParam
            . Orb.reqBody
            $ request
    }
