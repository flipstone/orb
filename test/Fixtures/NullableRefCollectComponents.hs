{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.NullableRefCollectComponents
  ( NullableRefCollectComponents (..)
  , nullableRefCollectComponentsOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

nullableRefCollectComponentsOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[NullableRefCollectComponents])
nullableRefCollectComponentsOpenApiRouter =
  Orb.provideOpenApi "nullable-ref-collect-components"
    . R.routeList
    $ (Orb.get (R.make NullableRefCollectComponents /- "nullable-ref-collect-components"))
      /: R.emptyRoutes

data NullableRefCollectComponents = NullableRefCollectComponents

instance Orb.HasHandler NullableRefCollectComponents where
  type HandlerResponses NullableRefCollectComponents = NullableRefCollectComponentsResponses
  type HandlerPermissionAction NullableRefCollectComponents = NoPermissions
  type HandlerMonad NullableRefCollectComponents = TDM.TestDispatchM
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "NullableRefCollectComponentsHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.requestQuery = Orb.EmptyRequestQuery
      , Orb.requestHeaders = Orb.EmptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 (FC.nullable nullableRefCollectComponentsResponseSchema)
            . Orb.addResponseSchema400 nullableRefCollectComponentsResponseSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.return200
              ( Right
                  NullableRefCollectComponentsResponse
                    { outerField =
                        InnerObject
                          { innerField = False
                          }
                    }
              )
      }

type NullableRefCollectComponentsResponses =
  [ Orb.Response200 (Either FC.Null NullableRefCollectComponentsResponse)
  , Orb.Response400 NullableRefCollectComponentsResponse
  , Orb.Response500 Orb.InternalServerError
  ]

newtype NullableRefCollectComponentsResponse = NullableRefCollectComponentsResponse {outerField :: InnerObject}

newtype InnerObject = InnerObject {innerField :: Bool}

nullableRefCollectComponentsResponseSchema :: FC.Fleece t => FC.Schema t NullableRefCollectComponentsResponse
nullableRefCollectComponentsResponseSchema =
  FC.object $
    FC.constructor NullableRefCollectComponentsResponse
      #+ FC.required "outerField" outerField innerObjectSchema

innerObjectSchema :: FC.Fleece t => FC.Schema t InnerObject
innerObjectSchema =
  FC.object $
    FC.constructor InnerObject
      #+ FC.required "innerField" innerField FC.boolean
