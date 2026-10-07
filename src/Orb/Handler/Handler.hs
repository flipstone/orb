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
  , HandlerRequest (..)
  , PermissionRequest (..)
  , runHandler
  , HasHandler (..)
  , NoRequestBody (..)
  , RequestBody (..)
  , schemaRequestBody
  , schemaRequestBodyWith
  , rawRequestBody
  , rawRequestBodyWith
  , formDataRequestBody
  , formDataRequestBodyWith
  , emptyRequestBody
  , BodyHandle
  , decodeBody
  , NoRequestQuery (..)
  , RequestQuery (..)
  , schemaRequestQuery
  , schemaRequestQueryWith
  , emptyRequestQuery
  , NoRequestHeaders (..)
  , RequestHeaders (..)
  , schemaRequestHeaders
  , schemaRequestHeadersWith
  , emptyRequestHeaders
  , encodeResponse
  )
where

import Beeline.Params qualified as BP
import Control.Concurrent.MVar qualified as MVar
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

data PermissionRequest route = PermissionRequest
  { permissionRequestRoute :: route
  , permissionRequestBody :: BodyHandle (S.TaggedUnion (HandlerResponses route)) (HandlerRequestBody route)
  , permissionRequestQuery :: HandlerRequestQuery route
  , permissionRequestHeaders :: HandlerRequestHeaders route
  }

data Handler route = Handler
  { handlerId :: String
  , requestBody :: RequestBody (HandlerRequestBody route) (HandlerResponses route)
  , handlerResponseBodies :: Response.ResponseBodies (HandlerResponses route)
  , requestQuery :: RequestQuery (HandlerRequestQuery route) (HandlerResponses route)
  , requestHeaders :: RequestHeaders (HandlerRequestHeaders route) (HandlerResponses route)
  , mkPermissionAction ::
      PermissionRequest route ->
      HandlerPermissionAction route
  , handleRequest ::
      HandlerRequest route ->
      PA.PermissionActionResult (HandlerPermissionAction route) ->
      HandlerMonad route (S.TaggedUnion (HandlerResponses route))
  }

class
  ( PA.PermissionAction (HandlerPermissionAction route)
  , PE.PermissionErrorConstraints
      (PA.PermissionActionError (HandlerPermissionAction route))
      (HandlerResponses route)
  , Response.Has500Response (HandlerResponses route)
  , PA.PermissionActionHandlerMonad (HandlerPermissionAction route) ~ HandlerMonad route
  , PE.PermissionErrorMonad (PA.PermissionActionError (HandlerPermissionAction route)) ~ PA.PermissionActionMonad (HandlerPermissionAction route)
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

  {- |
    'HandlerMonad' is an associated type that specifies the monad
    in which the route handler operates.
  -}
  type HandlerMonad route :: Kind.Type -> Kind.Type

  routeHandler :: Handler route

data NoRequestBody
  = NoRequestBody

data RequestBody body tags where
  RequestBody ::
    (forall t. FC.Fleece t => Maybe (FC.Schema t body)) ->
    (Wai.Request -> IO (Either (S.TaggedUnion tags) body)) ->
    RequestBody body tags
  Deferred ::
    RequestBody body tags ->
    RequestBody (BodyHandle (S.TaggedUnion tags) body) tags

schemaRequestBody ::
  Response.Has422Response tags =>
  (forall t. FC.Fleece t => FC.Schema t body) ->
  RequestBody body tags
schemaRequestBody =
  schemaRequestBodyWith
    (Response.mkResponse Response.status422 . Response.UnprocessableContentMessage)

schemaRequestBodyWith ::
  (T.Text -> S.TaggedUnion tags) ->
  (forall t. FC.Fleece t => FC.Schema t body) ->
  RequestBody body tags
schemaRequestBodyWith mkErrorResponse schema =
  RequestBody
    (Just schema)
    (parseBodyRequestSchema schema mkErrorResponse)

rawRequestBody ::
  Response.HasResponseCodeWithType tags "422" err =>
  (LBS.ByteString -> Either err body) ->
  RequestBody body tags
rawRequestBody =
  rawRequestBodyWith (Response.mkResponse Response.status422)

rawRequestBodyWith ::
  (err -> S.TaggedUnion tags) ->
  (LBS.ByteString -> Either err body) ->
  RequestBody body tags
rawRequestBodyWith mkErrorResponse decoder =
  RequestBody
    Nothing
    (parseBodyRaw mkErrorResponse decoder)

formDataRequestBody ::
  (Response.Has400Response tags, Response.HasResponseCodeWithType tags "422" err) =>
  (Form -> Either err body) ->
  RequestBody body tags
formDataRequestBody =
  formDataRequestBodyWith badRequest (Response.mkResponse Response.status422)

formDataRequestBodyWith ::
  (T.Text -> S.TaggedUnion tags) ->
  (err -> S.TaggedUnion tags) ->
  (Form -> Either err body) ->
  RequestBody body tags
formDataRequestBodyWith mkFormErrorResponse mkDecodeErrorResponse formDecoder =
  RequestBody
    Nothing
    (parseBodyFormData mkFormErrorResponse mkDecodeErrorResponse formDecoder)

emptyRequestBody :: RequestBody NoRequestBody tags
emptyRequestBody =
  RequestBody
    Nothing
    (const (pure (Right NoRequestBody)))

newtype BodyHandle err body
  = BodyHandle (IO (Either err body))

decodeBody :: MIO.MonadIO m => BodyHandle err body -> m (Either err body)
decodeBody (BodyHandle decode) =
  MIO.liftIO decode

newBodyHandle :: IO (Either err body) -> IO (BodyHandle err body)
newBodyHandle decode = do
  cacheVar <- MVar.newMVar Nothing
  pure . BodyHandle . MVar.modifyMVar cacheVar $ \cached ->
    case cached of
      Just result ->
        pure (cached, result)
      Nothing -> do
        result <- decode
        pure (Just result, result)

data NoRequestQuery
  = NoRequestQuery

data RequestQuery query tags = RequestQuery
  { requestQuerySchema :: forall schema. BP.QuerySchema schema => Maybe (schema query query)
  , requestQueryParser :: Wai.Request -> Either (S.TaggedUnion tags) query
  }

schemaRequestQuery ::
  Response.Has400Response tags =>
  (forall schema. BP.QuerySchema schema => schema query query) ->
  RequestQuery query tags
schemaRequestQuery =
  schemaRequestQueryWith badRequest

schemaRequestQueryWith ::
  (T.Text -> S.TaggedUnion tags) ->
  (forall schema. BP.QuerySchema schema => schema query query) ->
  RequestQuery query tags
schemaRequestQueryWith mkErrorResponse schema =
  RequestQuery
    { requestQuerySchema = Just schema
    , requestQueryParser =
        either (Left . mkErrorResponse) Right
          . BP.decodeQuery schema
          . Wai.rawQueryString
    }

emptyRequestQuery :: RequestQuery NoRequestQuery tags
emptyRequestQuery =
  RequestQuery
    { requestQuerySchema = Nothing
    , requestQueryParser = const (Right NoRequestQuery)
    }

data NoRequestHeaders
  = NoRequestHeaders

data RequestHeaders headers tags = RequestHeaders
  { requestHeadersSchema :: forall schema. BP.HeaderSchema schema => Maybe (schema headers headers)
  , requestHeadersParser :: Wai.Request -> Either (S.TaggedUnion tags) headers
  }

schemaRequestHeaders ::
  Response.Has400Response tags =>
  (forall schema. BP.HeaderSchema schema => schema headers headers) ->
  RequestHeaders headers tags
schemaRequestHeaders =
  schemaRequestHeadersWith badRequest

schemaRequestHeadersWith ::
  (T.Text -> S.TaggedUnion tags) ->
  (forall schema. BP.HeaderSchema schema => schema headers headers) ->
  RequestHeaders headers tags
schemaRequestHeadersWith mkErrorResponse schema =
  RequestHeaders
    { requestHeadersSchema = Just schema
    , requestHeadersParser =
        either (Left . mkErrorResponse) Right
          . BP.decodeHeaders schema
          . Wai.requestHeaders
    }

emptyRequestHeaders :: RequestHeaders NoRequestHeaders tags
emptyRequestHeaders =
  RequestHeaders
    { requestHeadersSchema = Nothing
    , requestHeadersParser = const (Right NoRequestHeaders)
    }

badRequest :: Response.Has400Response tags => T.Text -> S.TaggedUnion tags
badRequest =
  Response.mkResponse Response.status400 . Response.BadRequestMessage

runHandler ::
  ( HasHandler route
  , HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , MIO.MonadIO m
  , PA.PermissionActionMonad (HandlerPermissionAction route) ~ m
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
            req <- HasRequest.request
            body <- MIO.liftIO . newBodyHandle $ parseRequestBody (requestBody handler) req
            runPermissionAction handler $
              PermissionRequest
                { permissionRequestRoute = route
                , permissionRequestBody = body
                , permissionRequestQuery = query
                , permissionRequestHeaders = headers
                }

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
readHeaders handler =
  requestHeadersParser (requestHeaders handler) <$> HasRequest.request

readQuery ::
  ( Monad m
  , HasRequest.HasRequest m
  , tags ~ HandlerResponses route
  ) =>
  Handler route ->
  m (Either (S.TaggedUnion tags) (HandlerRequestQuery route))
readQuery handler =
  requestQueryParser (requestQuery handler) <$> HasRequest.request

parseRequestBody ::
  RequestBody body tags ->
  Wai.Request ->
  IO (Either (S.TaggedUnion tags) body)
parseRequestBody body req =
  case body of
    RequestBody _schema parser ->
      parser req
    Deferred deferred ->
      Right <$> newBodyHandle (parseRequestBody deferred req)

runPermissionAction ::
  ( MIO.MonadIO m
  , HasHandler route
  , PA.PermissionActionMonad (HandlerPermissionAction route) ~ m
  ) =>
  Handler route ->
  PermissionRequest route ->
  m (S.TaggedUnion (HandlerResponses route))
runPermissionAction handler permissionRequest = do
  let
    permissionAction =
      mkPermissionAction handler permissionRequest

  errOrPermissionResult <- PA.checkPermissionAction permissionAction

  case errOrPermissionResult of
    Left err -> PE.returnPermissionError err
    Right permissionResult -> do
      errOrBody <- decodeBody (permissionRequestBody permissionRequest)
      case errOrBody of
        Left errResponse ->
          pure errResponse
        Right body ->
          let
            request =
              HandlerRequest
                { reqRoute = permissionRequestRoute permissionRequest
                , reqBody = body
                , reqQuery = permissionRequestQuery permissionRequest
                , reqHeaders = permissionRequestHeaders permissionRequest
                }
          in
            PA.runPermissionActionHandler
              permissionAction
              permissionResult
              (handleRequest handler request)

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
  (T.Text -> S.TaggedUnion tags) ->
  (err -> S.TaggedUnion tags) ->
  (Form -> Either err request) ->
  Wai.Request ->
  IO (Either (S.TaggedUnion tags) request)
parseBodyFormData mkFormErrorResponse mkDecodeErrorResponse formDecoder req = do
  errOrFormFields <-
    Safe.try $
      Wai.parseRequestBodyEx
        Wai.defaultParseRequestBodyOptions
        Wai.lbsBackEnd
        req

  pure $
    case errOrFormFields of
      Left (err :: Wai.RequestParseException) ->
        Left . mkFormErrorResponse . T.pack . show $ err
      Right formFields ->
        case getForm formFields of
          Left err ->
            Left . mkFormErrorResponse $ err
          Right form ->
            case formDecoder form of
              Left err -> Left . mkDecodeErrorResponse $ err
              Right request -> Right $ request

parseBodyRequestSchema ::
  (forall t. FC.Fleece t => FC.Schema t request) ->
  (T.Text -> S.TaggedUnion tags) ->
  Wai.Request ->
  IO (Either (S.TaggedUnion tags) request)
parseBodyRequestSchema schema mkErrorResponse req = do
  body <- Wai.consumeRequestBodyStrict req
  pure $
    case FA.decode schema body of
      Left err ->
        Left
          . mkErrorResponse
          . T.pack
          $ err
      Right request ->
        Right $ request

parseBodyRaw ::
  (err -> S.TaggedUnion tags) ->
  (LBS.ByteString -> Either err request) ->
  Wai.Request ->
  IO (Either (S.TaggedUnion tags) request)
parseBodyRaw mkErrorResponse requestDecoder req = do
  body <- Wai.consumeRequestBodyStrict req
  pure $
    case requestDecoder body of
      Left err ->
        Left . mkErrorResponse $ err
      Right request ->
        Right request

encodeResponse :: Response.ResponseBodies tags -> S.TaggedUnion tags -> Response.ResponseData
encodeResponse =
  S.dissectTaggedUnion . Response.encodeResponseBranches
