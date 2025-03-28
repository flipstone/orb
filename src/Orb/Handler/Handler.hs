{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Orb.Handler.Handler
  ( Handler (..)
  , runHandler
  , HasHandler (..)
  , NoRequestBody (..)
  , RequestBodySchema (..)
  , useRouteAsPermissionAction
  )
where

import Data.Kind qualified as Kind
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import Network.Wai qualified as Wai
import Shrubbery qualified as S
import UnliftIO qualified

import Orb.Handler.PermissionAction qualified as PA
import Orb.Handler.PermissionError qualified as PE
import Orb.HasRequest qualified as HasRequest
import Orb.HasRespond qualified as HasRespond
import Orb.Response qualified as Response

data Handler route = Handler
  { handlerId :: String
  , requestBodySchema :: RequestBodySchema (HandlerRequestBody route) (HandlerResponses route)
  , handlerResponseSchemas :: Response.ResponseSchemas (HandlerResponses route)
  , mkPermissionAction ::
      route ->
      HandlerRequestBody route ->
      HandlerPermissionAction route
  , handleRequest ::
      route ->
      HandlerRequestBody route ->
      HandlerPermissionResult route ->
      HandlerMonad route (S.TaggedUnion (HandlerResponses route))
  }

useRouteAsPermissionAction :: route -> request -> route
useRouteAsPermissionAction =
  const

class
  ( PA.PermissionAction (HandlerPermissionAction route)
  , PE.PermissionErrorConstraints
      (PA.PermissionActionError (HandlerPermissionAction route))
      (HandlerResponses route)
  , Response.Has500Response (HandlerResponses route)
  , PA.PermissionActionMonad (HandlerPermissionAction route) ~ HandlerMonad route
  , PE.PermissionErrorMonad (PA.PermissionActionError (HandlerPermissionAction route)) ~ HandlerMonad route
  ) =>
  HasHandler route
  where
  type HandlerRequestBody route :: Kind.Type
  type HandlerResponses route :: [S.Tag]
  type HandlerPermissionAction route :: Kind.Type

  -- |
  --     'HandlerMonad' is an associated type that specifies the monad
  --     in which the route handler operates.
  type HandlerMonad route :: Kind.Type -> Kind.Type

  routeHandler :: Handler route

type HandlerPermissionResult route =
  PA.PermissionActionResult (HandlerPermissionAction route)

data NoRequestBody
  = NoRequestBody

data RequestBodySchema body tags where
  RequestBodySchema ::
    Response.Has422Response tags =>
    (forall schema. FC.Fleece schema => schema body) ->
    RequestBodySchema body tags
  NoRequestBodySchema ::
    RequestBodySchema NoRequestBody tags

runHandler ::
  ( HasHandler route
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  , HandlerMonad route ~ m
  ) =>
  Handler route ->
  route ->
  m Wai.ResponseReceived
runHandler handler route =
  case requestBodySchema handler of
    NoRequestBodySchema ->
      noRequestBodyHandler
        (handlerResponseSchemas handler)
        (runPermissionAction handler route NoRequestBody)
    RequestBodySchema bodySchema ->
      requestBodyHandler
        bodySchema
        (handlerResponseSchemas handler)
        (runPermissionAction handler route)

runPermissionAction ::
  (Monad m, HasHandler route, HandlerMonad route ~ m) =>
  Handler route ->
  route ->
  HandlerRequestBody route ->
  m (S.TaggedUnion (HandlerResponses route))
runPermissionAction handler route body = do
  let
    permissionAction =
      mkPermissionAction handler route body

  errOrPermissionResult <- PA.checkPermissionAction permissionAction

  case errOrPermissionResult of
    Left err -> PE.returnPermissionError err
    Right permissionResult -> handleRequest handler route body permissionResult

noRequestBodyHandler ::
  ( HasRespond.HasRespond m
  , Response.Has500Response tags
  , UnliftIO.MonadUnliftIO m
  ) =>
  Response.ResponseSchemas tags ->
  m (S.TaggedUnion tags) ->
  m Wai.ResponseReceived
noRequestBodyHandler schemas action = do
  errOrResponse <- UnliftIO.tryAny action
  response <-
    case errOrResponse of
      Right response -> pure response
      Left _exception -> do
        -- TODO: Implement logging such that it works for any Monad m
        --
        Response.return500 Response.InternalServerError

  let
    responseData = encodeResponse schemas response
    contentTypeHeader =
      maybeToList $
        ("Content-Type",)
          <$> Response.responseDataContentType responseData

  Response.respondWith $
    Wai.responseLBS
      (Response.responseDataStatus responseData)
      (contentTypeHeader <> Response.responseDataExtraHeaders responseData)
      (Response.responseDataBytes responseData)

requestBodyHandler ::
  ( Response.Has422Response tags
  , Response.Has500Response tags
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  ) =>
  FA.Decoder request ->
  Response.ResponseSchemas tags ->
  (request -> m (S.TaggedUnion tags)) ->
  m Wai.ResponseReceived
requestBodyHandler requestDecoder schemas action =
  noRequestBodyHandler schemas $ do
    req <- HasRequest.request
    body <- UnliftIO.liftIO $ Wai.consumeRequestBodyStrict req
    case FA.decode requestDecoder body of
      Left err ->
        Response.return422 . Response.UnprocessableContentMessage . T.pack $ err
      Right request ->
        action request

encodeResponse :: Response.ResponseSchemas tags -> S.TaggedUnion tags -> Response.ResponseData
encodeResponse =
  S.dissectTaggedUnion . Response.encodeResponseBranches
