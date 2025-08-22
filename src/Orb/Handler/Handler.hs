{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Orb.Handler.Handler
  ( Handler (..)
  , HandlerRequest (..)
  , runHandler
  , HasHandler (..)
  , NoRequestBody (..)
  , RequestBody (..)
  , NoRequestQuery (..)
  , RequestQuery (..)
  , NoRequestHeaders (..)
  , RequestHeaders (..)
  , encodeResponse
  )
where

import Beeline.Params qualified as BP
import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class qualified as MIO
import Data.ByteString.Lazy qualified as LBS
import Data.Kind qualified as Kind
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import Network.Wai qualified as Wai
import Network.Wai.Parse qualified as Wai
import Shrubbery qualified as S

import Orb.Handler.Form (Form, getForm)
import Orb.Handler.PermissionAction qualified as PA
import Orb.Handler.PermissionError qualified as PE
import Orb.HasLogger qualified as HasLogger
import Orb.HasRequest qualified as HasRequest
import Orb.HasRespond qualified as HasRespond
import Orb.Response qualified as Response

data HandlerRequest route = HandlerRequest
  { reqRoute :: route
  , reqBody :: HandlerRequestBody route
  , reqQuery :: HandlerRequestQuery route
  , reqHeaders :: HandlerRequestHeaders route
  }

data Handler route = Handler
  { handlerId :: String
  , requestBody :: RequestBody (HandlerRequestBody route) (HandlerResponses route)
  , handlerResponseBodies :: Response.ResponseBodies (HandlerResponses route)
  , requestQuery :: RequestQuery (HandlerRequestQuery route) (HandlerResponses route)
  , requestHeaders :: RequestHeaders (HandlerRequestHeaders route) (HandlerResponses route)
  , mkPermissionAction ::
      HandlerRequest route ->
      HandlerPermissionAction route
  , handleRequest ::
      HandlerRequest route ->
      HandlerPermissionResult route ->
      HandlerMonad route (S.TaggedUnion (HandlerResponses route))
  }

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
  type HandlerRequestBody route = NoRequestBody

  type HandlerRequestQuery route :: Kind.Type
  type HandlerRequestQuery route = NoRequestQuery

  type HandlerRequestHeaders route :: Kind.Type
  type HandlerRequestHeaders route = NoRequestHeaders

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
  SchemaRequestBody ::
    Response.Has422Response tags =>
    (forall schema. FC.Fleece schema => schema body) ->
    RequestBody body tags
  RawRequestBody ::
    Response.HasResponseCodeWithType tags "422" err =>
    (LBS.ByteString -> Either err body) ->
    RequestBody body tags
  FormDataRequestBody ::
    (Response.Has400Response tags, Response.HasResponseCodeWithType tags "422" err) =>
    (Form -> Either err body) ->
    RequestBody body tags
  EmptyRequestBody ::
    RequestBody NoRequestBody tags

data NoRequestQuery
  = NoRequestQuery

data RequestQuery query tags where
  RequestQuery ::
    Response.Has400Response tags =>
    (forall schema. BP.QuerySchema schema => schema query query) ->
    RequestQuery query tags
  EmptyRequestQuery ::
    RequestQuery NoRequestQuery tags

data NoRequestHeaders
  = NoRequestHeaders

data RequestHeaders headers tags where
  RequestHeaders ::
    Response.Has400Response tags =>
    (forall schema. BP.HeaderSchema schema => schema headers headers) ->
    RequestHeaders headers tags
  EmptyRequestHeaders ::
    RequestHeaders NoRequestHeaders tags

runHandler ::
  ( HasHandler route
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , MIO.MonadIO m
  , HandlerMonad route ~ m
  , Safe.MonadCatch m
  ) =>
  Handler route ->
  route ->
  m Wai.ResponseReceived
runHandler handler route = do
  response <- returnAnyExceptionAs500 $ do
    errOrHeaders <- readHeaders handler
    case errOrHeaders of
      Left errResponse ->
        pure errResponse
      Right headers -> do
        errOrQuery <- readQuery handler
        case errOrQuery of
          Left errResponse ->
            pure errResponse
          Right query -> do
            errOrBody <- readBody handler
            case errOrBody of
              Left errResponse -> pure errResponse
              Right body ->
                let
                  request =
                    HandlerRequest
                      { reqRoute = route
                      , reqBody = body
                      , reqQuery = query
                      , reqHeaders = headers
                      }
                in
                  runPermissionAction handler request

  let
    responseData =
      encodeResponse (handlerResponseBodies handler) response

    contentTypeHeader =
      maybeToList $
        ("Content-Type",)
          <$> Response.responseDataContentType responseData
    status = Response.responseDataStatus responseData
    headers = contentTypeHeader <> Response.responseDataExtraHeaders responseData

  Response.respondWith $
    case Response.responseDataContent responseData of
      Response.ResponseContentFile path mbPart -> Wai.responseFile status headers path mbPart
      Response.ResponseContentBuilder builder -> Wai.responseBuilder status headers builder
      Response.ResponseContentStream streamingBody -> Wai.responseStream status headers streamingBody

readHeaders ::
  ( Monad m
  , HasRequest.HasRequest m
  , tags ~ HandlerResponses route
  ) =>
  Handler route ->
  m (Either (S.TaggedUnion tags) (HandlerRequestHeaders route))
readHeaders handler = do
  request <- HasRequest.request
  case requestHeaders handler of
    EmptyRequestHeaders ->
      pure (Right NoRequestHeaders)
    RequestHeaders schema ->
      case BP.decodeHeaders schema (Wai.requestHeaders request) of
        Left err -> fmap Left . Response.return400 . Response.BadRequestMessage $ err
        Right headers -> pure . Right $ headers

readQuery ::
  ( Monad m
  , HasRequest.HasRequest m
  , tags ~ HandlerResponses route
  ) =>
  Handler route ->
  m (Either (S.TaggedUnion tags) (HandlerRequestQuery route))
readQuery handler = do
  request <- HasRequest.request
  case requestQuery handler of
    EmptyRequestQuery ->
      pure (Right NoRequestQuery)
    RequestQuery schema ->
      case BP.decodeQuery schema (Wai.rawQueryString request) of
        Left err -> fmap Left . Response.return400 . Response.BadRequestMessage $ err
        Right query -> pure . Right $ query

readBody ::
  ( MIO.MonadIO m
  , HasRequest.HasRequest m
  , tags ~ HandlerResponses route
  ) =>
  Handler route ->
  m (Either (S.TaggedUnion tags) (HandlerRequestBody route))
readBody handler =
  case requestBody handler of
    SchemaRequestBody schema -> parseBodyRequestSchema schema
    RawRequestBody bodyDecoder -> parseBodyRaw bodyDecoder
    FormDataRequestBody formDecoder -> parseBodyFormData formDecoder
    EmptyRequestBody -> pure . Right $ NoRequestBody

runPermissionAction ::
  (Monad m, HasHandler route, HandlerMonad route ~ m) =>
  Handler route ->
  HandlerRequest route ->
  m (S.TaggedUnion (HandlerResponses route))
runPermissionAction handler request = do
  let
    permissionAction =
      mkPermissionAction handler request

  errOrPermissionResult <- PA.checkPermissionAction permissionAction

  case errOrPermissionResult of
    Left err -> PE.returnPermissionError err
    Right permissionResult -> handleRequest handler request permissionResult

returnAnyExceptionAs500 ::
  ( MIO.MonadIO m
  , Safe.MonadCatch m
  , HasLogger.HasLogger m
  , HasRespond.HasRespond m
  , Response.Has500Response tags
  ) =>
  m (S.TaggedUnion tags) ->
  m (S.TaggedUnion tags)
returnAnyExceptionAs500 action = do
  errOrResponse <- Safe.tryAny action
  case errOrResponse of
    Right response -> pure response
    Left exception -> do
      HasLogger.log exception
      Response.return500 Response.InternalServerError

parseBodyFormData ::
  ( Response.Has400Response tags
  , Response.HasResponseCodeWithType tags "422" err
  , HasRequest.HasRequest m
  , MIO.MonadIO m
  ) =>
  (Form -> Either err request) ->
  m (Either (S.TaggedUnion tags) request)
parseBodyFormData requestDecoder = do
  req <- HasRequest.request
  errOrFormFields <-
    MIO.liftIO
      . Safe.try
      $ Wai.parseRequestBodyEx
        Wai.defaultParseRequestBodyOptions
        Wai.lbsBackEnd
        req

  case errOrFormFields of
    Left (err :: Wai.RequestParseException) ->
      fmap Left . Response.return400 . Response.BadRequestMessage . T.pack . show $ err
    Right formFields ->
      case getForm formFields of
        Left err ->
          fmap Left . Response.return400 . Response.BadRequestMessage $ err
        Right form ->
          case requestDecoder form of
            Left err -> fmap Left . Response.return422 $ err
            Right request -> pure . Right $ request

parseBodyRequestSchema ::
  ( Response.Has422Response tags
  , HasRequest.HasRequest m
  , MIO.MonadIO m
  ) =>
  (forall schema. FC.Fleece schema => schema request) ->
  m (Either (S.TaggedUnion tags) request)
parseBodyRequestSchema schema = do
  req <- HasRequest.request
  body <- MIO.liftIO $ Wai.consumeRequestBodyStrict req
  case FA.decode schema body of
    Left err ->
      fmap Left
        . Response.return422
        . Response.UnprocessableContentMessage
        . T.pack
        $ err
    Right request ->
      pure . Right $ request

parseBodyRaw ::
  ( Response.HasResponseCodeWithType tags "422" err
  , HasRequest.HasRequest m
  , MIO.MonadIO m
  ) =>
  (LBS.ByteString -> Either err request) ->
  m (Either (S.TaggedUnion tags) request)
parseBodyRaw requestDecoder = do
  req <- HasRequest.request
  body <- MIO.liftIO $ Wai.consumeRequestBodyStrict req
  case requestDecoder body of
    Left err -> fmap Left . Response.return422 $ err
    Right request -> pure . Right $ request

encodeResponse :: Response.ResponseBodies tags -> S.TaggedUnion tags -> Response.ResponseData
encodeResponse =
  S.dissectTaggedUnion . Response.encodeResponseBranches
