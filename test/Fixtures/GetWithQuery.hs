{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithQuery
  ( GetWithQuery (..)
  , TestQuery (..)
  ) where

import Beeline.HTTP.Client ((?+))
import Beeline.HTTP.Client qualified as Client
import Beeline.Routing qualified as R
import Data.Int qualified as Int
import Data.Text qualified as T

import Fixtures.NoPermissions as Export
import Orb qualified
import TestDispatchM qualified as TDM

data GetWithQuery = GetWithQuery

data TestQuery = TestQuery
  { textQueryParam :: T.Text
  , int8QueryParam :: Maybe Int.Int8
  }

testQuerySchema :: Client.ParameterCollectionSchema q => q TestQuery TestQuery
testQuerySchema =
  Client.makeParams TestQuery
    ?+ Client.required textQueryParam (R.textParam "textQueryParam")
    ?+ Client.optional int8QueryParam (R.int8Param "int8QueryParam")

instance Orb.HasHandler GetWithQuery where
  type HandlerRequestBody GetWithQuery = Orb.NoRequestBody
  type HandlerRequestQuery GetWithQuery = TestQuery
  type HandlerRequestHeaders GetWithQuery = Orb.NoRequestHeaders
  type HandlerResponses GetWithQuery = TestResponses
  type HandlerPermissionAction GetWithQuery = NoPermissions
  type HandlerMonad GetWithQuery = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response400 Orb.BadRequestMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithQuery
handler =
  Orb.Handler
    { Orb.handlerId = "getWithQuery"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.RequestQuery testQuerySchema
    , Orb.requestHeaders = Orb.EmptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 Orb.successMessageSchema
          . Orb.addResponseSchema400 Orb.badRequestMessageSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \request () ->
          let
            q = textQueryParam . Orb.reqQuery $ request
          in
            Orb.return200 (Orb.SuccessMessage q)
    }
