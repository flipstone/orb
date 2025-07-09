{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fixtures
  ( TestRoute1 (TestRoute1)
  , TestRoute2 (TestRoute2)
  , mkTestHandler
  , TestResponses
  , basicOpenApiRouter
  , nullableRefOpenApiRouter
  , unionOpenApiRouter
  , nullableRefCollectComponentsOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Data.Void qualified as Void
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Orb qualified

basicOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union [TestRoute1, TestRoute2])
basicOpenApiRouter =
  Orb.provideOpenApi "basic-open-api"
    . R.routeList
    $ Orb.provideOpenApi "just-route-1" (Orb.get (R.make TestRoute1 /- "test/route1"))
      /: Orb.get (R.make TestRoute2 /- "test/route2")
      /: R.emptyRoutes

unionOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[Union])
unionOpenApiRouter =
  Orb.provideOpenApi "union"
    . R.routeList
    $ (Orb.get (R.make Union /- "union"))
      /: R.emptyRoutes

nullableRefOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[NullableRef])
nullableRefOpenApiRouter =
  Orb.provideOpenApi "nullable-ref"
    . R.routeList
    $ (Orb.get (R.make NullableRef /- "nullable-ref"))
      /: R.emptyRoutes

nullableRefCollectComponentsOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[NullableRefCollectComponents])
nullableRefCollectComponentsOpenApiRouter =
  Orb.provideOpenApi "nullable-ref-collect-components"
    . R.routeList
    $ (Orb.get (R.make NullableRefCollectComponents /- "nullable-ref-collect-components"))
      /: R.emptyRoutes

-- Nullable Ref

data NullableRef = NullableRef

instance Orb.HasHandler NullableRef where
  type HandlerRequestBody NullableRef = Orb.NoRequestBody
  type HandlerResponses NullableRef = NullableRefResponses
  type HandlerPermissionAction NullableRef = NoPermissions
  type HandlerMonad NullableRef = IO
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "NullableRefHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 (FC.nullable nullableRefResponseSchema)
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_route _request -> NoPermissions
      , Orb.handleRequest =
          \_route Orb.NoRequestBody () ->
            Orb.return200
              ( Right
                  NullableRefResponse
                    { boolField = False
                    , nullableBoolField = Right False
                    , validatedNullableBoolField = Right False
                    , nullableValidatedBoolField = Right False
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
  }

nullableRefResponseSchema :: FC.Fleece schema => schema NullableRefResponse
nullableRefResponseSchema =
  FC.object $
    FC.constructor NullableRefResponse
      #+ FC.required "boolField" boolField FC.boolean
      #+ FC.required "nullableBoolField" nullableBoolField (FC.nullable FC.boolean)
      #+ FC.required "validatedNullableBoolField" validatedNullableBoolField (FC.transformNamed "MyNullableBool" id id (FC.nullable FC.boolean))
      #+ FC.required "nullableValidatedBoolField" nullableValidatedBoolField (FC.nullable (FC.transformNamed "MyBool" id id FC.boolean))

-- Nullable Ref Collect Components

data NullableRefCollectComponents = NullableRefCollectComponents

instance Orb.HasHandler NullableRefCollectComponents where
  type HandlerRequestBody NullableRefCollectComponents = Orb.NoRequestBody
  type HandlerResponses NullableRefCollectComponents = NullableRefCollectComponentsResponses
  type HandlerPermissionAction NullableRefCollectComponents = NoPermissions
  type HandlerMonad NullableRefCollectComponents = IO
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "NullableRefCollectComponentsHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 (FC.nullable nullableRefCollectComponentsResponseSchema)
            . Orb.addResponseSchema400 nullableRefCollectComponentsResponseSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_route _request -> NoPermissions
      , Orb.handleRequest =
          \_route Orb.NoRequestBody () ->
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

nullableRefCollectComponentsResponseSchema :: FC.Fleece schema => schema NullableRefCollectComponentsResponse
nullableRefCollectComponentsResponseSchema =
  FC.object $
    FC.constructor NullableRefCollectComponentsResponse
      #+ FC.required "outerField" outerField innerObjectSchema

innerObjectSchema :: FC.Fleece schema => schema InnerObject
innerObjectSchema =
  FC.object $
    FC.constructor InnerObject
      #+ FC.required "innerField" innerField FC.boolean

-- Test Route 1

data TestRoute1 = TestRoute1

instance Orb.HasHandler TestRoute1 where
  type HandlerRequestBody TestRoute1 = Orb.NoRequestBody
  type HandlerResponses TestRoute1 = TestResponses
  type HandlerPermissionAction TestRoute1 = NoPermissions
  type HandlerMonad TestRoute1 = IO
  routeHandler = mkTestHandler "testRoute1"

data TestRoute2 = TestRoute2

instance Orb.HasHandler TestRoute2 where
  type HandlerRequestBody TestRoute2 = Orb.NoRequestBody
  type HandlerResponses TestRoute2 = TestResponses
  type HandlerPermissionAction TestRoute2 = NoPermissions
  type HandlerMonad TestRoute2 = IO
  routeHandler = mkTestHandler "testRoute2"

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response500 Orb.InternalServerError
  ]

mkTestHandler ::
  ( Orb.HandlerPermissionAction route ~ NoPermissions
  , Orb.HandlerResponses route ~ TestResponses
  , Orb.HandlerRequestBody route ~ Orb.NoRequestBody
  , Applicative (Orb.HandlerMonad route)
  ) =>
  String ->
  Orb.Handler route
mkTestHandler handlerId =
  Orb.Handler
    { Orb.handlerId = handlerId
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_route _request -> NoPermissions
    , Orb.handleRequest =
        \_route Orb.NoRequestBody () ->
          Orb.return200 (Orb.SuccessMessage "Hi")
    }

data NoPermissions
  = NoPermissions

newtype NoError = NoError Void.Void

instance Orb.PermissionAction NoPermissions where
  type PermissionActionMonad NoPermissions = IO
  type PermissionActionError NoPermissions = NoError
  type PermissionActionResult NoPermissions = ()

  checkPermissionAction _ =
    pure (Right ())

instance Orb.PermissionError NoError where
  type PermissionErrorConstraints NoError _tags = ()
  type PermissionErrorMonad NoError = IO

  returnPermissionError (NoError void) =
    Void.absurd void

-- Union

data Union = Union

instance Orb.HasHandler Union where
  type HandlerRequestBody Union = Orb.NoRequestBody
  type HandlerResponses Union = UnionResponses
  type HandlerPermissionAction Union = NoPermissions
  type HandlerMonad Union = IO
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "UnionHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 unionResponseSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_route _request -> NoPermissions
      , Orb.handleRequest =
          \_route Orb.NoRequestBody () ->
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
