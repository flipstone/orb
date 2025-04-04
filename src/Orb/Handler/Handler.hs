{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Orb.Handler.Handler
  ( Handler (..)
  , runHandler
  , HasHandler (..)
  , NoRequestBody (..)
  , RequestBody (..)
  , useRouteAsPermissionAction
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Kind qualified as Kind
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import Network.Wai qualified as Wai
import Network.Wai.Parse qualified as Wai
import Shrubbery qualified as S
import UnliftIO qualified

import Orb.Handler.Form (Form, getForm)
import Orb.Handler.PermissionAction qualified as PA
import Orb.Handler.PermissionError qualified as PE
import Orb.HasLogger qualified as HasLogger
import Orb.HasRequest qualified as HasRequest
import Orb.HasRespond qualified as HasRespond
import Orb.Response qualified as Response

data Handler route = Handler
  { handlerId :: String
  , requestBody :: RequestBody (HandlerRequestBody route) (HandlerResponses route)
  , handlerResponseBodies :: Response.ResponseBodies (HandlerResponses route)
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

data RequestBody body tags where
  RequestSchema ::
    Response.Has422Response tags =>
    (forall schema. FC.Fleece schema => schema body) ->
    RequestBody body tags
  RequestRawBody ::
    Response.HasResponseCodeWithType tags "422" err =>
    (LBS.ByteString -> Either err body) ->
    RequestBody body tags
  RequestFormData ::
    (Response.Has400Response tags, Response.HasResponseCodeWithType tags "422" err) =>
    (Form -> Either err body) ->
    RequestBody body tags
  EmptyRequestBody ::
    RequestBody NoRequestBody tags

runHandler ::
  ( HasHandler route
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  , HandlerMonad route ~ m
  ) =>
  Handler route ->
  route ->
  m Wai.ResponseReceived
runHandler handler route =
  case requestBody handler of
    RequestSchema schema ->
      requestSchemaHandler
        schema
        (handlerResponseBodies handler)
        (runPermissionAction handler route)
    RequestRawBody bodyDecoder ->
      requestBodyHandler
        bodyDecoder
        (handlerResponseBodies handler)
        (runPermissionAction handler route)
    RequestFormData formDecoder ->
      requestFormDataHandler
        formDecoder
        (handlerResponseBodies handler)
        (runPermissionAction handler route)
    EmptyRequestBody ->
      emptyRequestBodyHandler
        (handlerResponseBodies handler)
        (runPermissionAction handler route NoRequestBody)

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

emptyRequestBodyHandler ::
  ( HasLogger.HasLogger m
  , HasRespond.HasRespond m
  , Response.Has500Response tags
  , UnliftIO.MonadUnliftIO m
  ) =>
  Response.ResponseBodies tags ->
  m (S.TaggedUnion tags) ->
  m Wai.ResponseReceived
emptyRequestBodyHandler bodies action = do
  errOrResponse <- UnliftIO.tryAny action
  response <-
    case errOrResponse of
      Right response -> pure response
      Left exception -> do
        HasLogger.log exception
        Response.return500 Response.InternalServerError
  let
    responseData = encodeResponse bodies response
    contentTypeHeader =
      maybeToList $
        ("Content-Type",)
          <$> Response.responseDataContentType responseData

  Response.respondWith $
    Wai.responseLBS
      (Response.responseDataStatus responseData)
      (contentTypeHeader <> Response.responseDataExtraHeaders responseData)
      (Response.responseDataBytes responseData)

requestFormDataHandler ::
  ( Response.Has400Response tags
  , Response.HasResponseCodeWithType tags "422" err
  , Response.Has500Response tags
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  ) =>
  (Form -> Either err request) ->
  Response.ResponseBodies tags ->
  (request -> m (S.TaggedUnion tags)) ->
  m Wai.ResponseReceived
requestFormDataHandler requestDecoder bodies action =
  emptyRequestBodyHandler bodies $ do
    req <- HasRequest.request
    errOrFormFields <-
      UnliftIO.liftIO
        . UnliftIO.try
        $ Wai.parseRequestBodyEx
          Wai.defaultParseRequestBodyOptions
          Wai.lbsBackEnd
          req

    case errOrFormFields of
      Left (err :: Wai.RequestParseException) ->
        Response.return400 . Response.BadRequestMessage . T.pack $ show err
      Right formFields ->
        case getForm formFields of
          Left err ->
            Response.return400 $ Response.BadRequestMessage err
          Right form ->
            case requestDecoder form of
              Left err -> Response.return422 err
              Right request -> action request

requestSchemaHandler ::
  ( Response.Has422Response tags
  , Response.Has500Response tags
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  ) =>
  (forall schema. FC.Fleece schema => schema request) ->
  Response.ResponseBodies tags ->
  (request -> m (S.TaggedUnion tags)) ->
  m Wai.ResponseReceived
requestSchemaHandler schema bodies action =
  emptyRequestBodyHandler bodies $ do
    req <- HasRequest.request
    body <- UnliftIO.liftIO $ Wai.consumeRequestBodyStrict req
    case FA.decode schema body of
      Left err ->
        Response.return422 . Response.UnprocessableContentMessage $ T.pack err
      Right request ->
        action request

requestBodyHandler ::
  ( Response.HasResponseCodeWithType tags "422" err
  , Response.Has500Response tags
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , UnliftIO.MonadUnliftIO m
  ) =>
  (LBS.ByteString -> Either err request) ->
  Response.ResponseBodies tags ->
  (request -> m (S.TaggedUnion tags)) ->
  m Wai.ResponseReceived
requestBodyHandler requestDecoder bodies action =
  emptyRequestBodyHandler bodies $ do
    req <- HasRequest.request
    body <- UnliftIO.liftIO $ Wai.consumeRequestBodyStrict req
    case requestDecoder body of
      Left err -> Response.return422 err
      Right request -> action request

encodeResponse :: Response.ResponseBodies tags -> S.TaggedUnion tags -> Response.ResponseData
encodeResponse =
  S.dissectTaggedUnion . Response.encodeResponseBranches
