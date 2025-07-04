{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.GetWithHeaders
  ( GetWithHeaders (..)
  , TestHeaders (..)
  ) where

import Beeline.HTTP.Client ((?+))
import Beeline.HTTP.Client qualified as Client
import Beeline.Routing qualified as R
import Data.Text qualified as T

import Fixtures.NoPermissions as Export
import Orb qualified
import TestDispatchM qualified as TDM

data GetWithHeaders = GetWithHeaders

newtype TestHeaders = TestHeaders
  { headerParam :: T.Text
  }

testHeadersSchema :: Client.ParameterCollectionSchema q => q TestHeaders TestHeaders
testHeadersSchema =
  Client.makeParams TestHeaders
    ?+ Client.required headerParam (R.textParam "headerParam")

instance Orb.HasHandler GetWithHeaders where
  type HandlerRequestBody GetWithHeaders = Orb.NoRequestBody
  type HandlerRequestQuery GetWithHeaders = Orb.NoRequestQuery
  type HandlerRequestHeaders GetWithHeaders = TestHeaders
  type HandlerResponses GetWithHeaders = TestResponses
  type HandlerPermissionAction GetWithHeaders = NoPermissions
  type HandlerMonad GetWithHeaders = TDM.TestDispatchM
  routeHandler = handler

type TestResponses =
  [ Orb.Response200 Orb.SuccessMessage
  , Orb.Response400 Orb.BadRequestMessage
  , Orb.Response500 Orb.InternalServerError
  ]

handler :: Orb.Handler GetWithHeaders
handler =
  Orb.Handler
    { Orb.handlerId = "getWithHeaders"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.RequestHeaders testHeadersSchema
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
            q = headerParam . Orb.reqHeaders $ request
          in
            Orb.return200 (Orb.SuccessMessage q)
    }
