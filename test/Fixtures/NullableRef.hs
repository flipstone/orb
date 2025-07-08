{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.NullableRef
  ( NullableRef (..)
  ) where

import Fleece.Core ((#+))
import Fleece.Core qualified as FC

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

data NullableRef = NullableRef

instance Orb.HasHandler NullableRef where
  type HandlerRequestBody NullableRef = Orb.NoRequestBody
  type HandlerRequestQuery NullableRef = Orb.NoRequestQuery
  type HandlerRequestHeaders NullableRef = Orb.NoRequestHeaders
  type HandlerResponses NullableRef = NullableRefResponses
  type HandlerPermissionAction NullableRef = NoPermissions
  type HandlerMonad NullableRef = TDM.TestDispatchM
  routeHandler = handler

type NullableRefResponses =
  [ Orb.Response200 (Either FC.Null NullableRefResponse)
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler NullableRef
handler =
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
          Orb.return200 (Right $ NullableRefResponse 42)
    }

newtype NullableRefResponse = NullableRefResponse {unNullableRefResponse :: Int}

nullableRefResponseSchema :: FC.Fleece schema => schema NullableRefResponse
nullableRefResponseSchema =
  FC.objectNamed "WrappedInteger" $
    FC.constructor NullableRefResponse
      #+ FC.required "field" unNullableRefResponse FC.int
