{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.NullableRef
  ( NullableRef (..)
  , nullableRefOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

nullableRefOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[NullableRef])
nullableRefOpenApiRouter =
  Orb.provideOpenApi "nullable-ref"
    . R.routeList
    $ (Orb.get (R.make NullableRef /- "nullable-ref"))
      /: R.emptyRoutes

data NullableRef = NullableRef

instance Orb.HasHandler NullableRef where
  type HandlerResponses NullableRef = NullableRefResponses
  type HandlerPermissionAction NullableRef = NoPermissions
  type HandlerMonad NullableRef = TDM.TestDispatchM
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "NullableRefHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.requestQuery = Orb.EmptyRequestQuery
      , Orb.requestHeaders = Orb.EmptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 (FC.nullable nullableRefResponseSchema)
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.return200
              ( Right
                  NullableRefResponse
                    { boolField = False
                    , nullableBoolField = Right False
                    , validatedNullableBoolField = Right False
                    , nullableValidatedBoolField = Right False
                    , nullableArrayField = Right []
                    , arrayOfNullableField = []
                    }
              )
      }

type NullableRefResponses =
  [ Orb.Response200 (Either FC.Null NullableRefResponse)
  , Orb.Response500 Orb.InternalServerError
  ]

data NullableRefResponse = NullableRefResponse
  { boolField :: Bool
  , nullableBoolField :: Either FC.Null Bool
  , validatedNullableBoolField :: Either FC.Null Bool
  , nullableValidatedBoolField :: Either FC.Null Bool
  , nullableArrayField :: Either FC.Null [Bool]
  , arrayOfNullableField :: [Either FC.Null Bool]
  }

nullableRefResponseSchema :: FC.Fleece t => FC.Schema t NullableRefResponse
nullableRefResponseSchema =
  FC.object $
    FC.constructor NullableRefResponse
      #+ FC.required "boolField" boolField FC.boolean
      #+ FC.required "nullableBoolField" nullableBoolField (FC.nullable FC.boolean)
      #+ FC.required "validatedNullableBoolField" validatedNullableBoolField (FC.transformNamed "MyNullableBool" id id (FC.nullable FC.boolean))
      #+ FC.required "nullableValidatedBoolField" nullableValidatedBoolField (FC.nullable (FC.transformNamed "MyBool" id id FC.boolean))
      #+ FC.required "nullableArrayField" nullableArrayField (FC.nullable (FC.list FC.boolean))
      #+ FC.required "arrayOfNullableField" arrayOfNullableField (FC.list (FC.nullable FC.boolean))
