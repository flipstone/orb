{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.Union
  ( Union (..)
  , unionOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+), (#|))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

unionOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[Union])
unionOpenApiRouter =
  Orb.provideOpenApi "union"
    . R.routeList
    $ (Orb.get (R.make Union /- "union"))
      /: R.emptyRoutes

data Union = Union

instance Orb.HasHandler Union where
  type HandlerResponses Union = UnionResponses
  type HandlerPermissionAction Union = NoPermissions
  type HandlerMonad Union = TDM.TestDispatchM
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "UnionHandler"
      , Orb.requestBody = Orb.emptyRequestBody
      , Orb.requestQuery = Orb.emptyRequestQuery
      , Orb.requestHeaders = Orb.emptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema Orb.status200 unionResponseSchema
            . Orb.addResponseSchema Orb.status500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.returnResponse Orb.status200 (S.unify @Int 42)
      }

type UnionResponses =
  [ Orb.Response "200" UnionResponse
  , Orb.Response "500" Orb.InternalServerError
  ]

type UnionResponse = S.Union [Int, RandomObject]

unionResponseSchema :: FC.Fleece t => FC.Schema t UnionResponse
unionResponseSchema =
  FC.unionNamed "UnionResponse" $
    FC.unionMember FC.int
      #| FC.unionMember randomObjectSchema

data RandomObject = RandomObject
  { randomBool :: Bool
  , randomText :: T.Text
  }

randomObjectSchema :: FC.Fleece t => FC.Schema t RandomObject
randomObjectSchema =
  FC.object $
    FC.constructor RandomObject
      #+ FC.required "bool" randomBool FC.boolean
      #+ FC.required "text" randomText FC.text
