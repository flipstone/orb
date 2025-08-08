{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.Union
  ( Union (..)
  , unionOpenApiRouter
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
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.requestQuery = Orb.EmptyRequestQuery
      , Orb.requestHeaders = Orb.EmptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 unionResponseSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.return200 (UnionResponse (Left 42))
      }

type UnionResponses =
  [ Orb.Response200 UnionResponse
  , Orb.Response500 Orb.InternalServerError
  ]

newtype UnionResponse = UnionResponse (Either Int RandomObject)

unionResponseSchema :: FC.Fleece schema => schema UnionResponse
unionResponseSchema =
  FC.coerceSchema intOrObjectSchema

intOrObjectSchema :: FC.Fleece schema => schema (Either Int RandomObject)
intOrObjectSchema =
  FC.eitherOfNamed "IntOrObject" FC.int randomObjectSchema

data RandomObject = RandomObject
  { randomBool :: Bool
  , randomText :: T.Text
  }

randomObjectSchema :: FC.Fleece schema => schema RandomObject
randomObjectSchema =
  FC.object $
    FC.constructor RandomObject
      #+ FC.required "bool" randomBool FC.boolean
      #+ FC.required "text" randomText FC.text
