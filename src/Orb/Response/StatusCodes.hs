{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Orb.Response.StatusCodes
  ( addResponseSchema200
  , addResponseDocument200
  , addResponseSchema201
  , addResponseSchema204
  , addResponseSchema400
  , addResponseSchema401
  , addResponseSchema403
  , addResponseSchema404
  , addResponseSchema409
  , addResponseSchema422
  , addResponseSchema500
  , addResponseSchema503
  , Response200
  , Response201
  , Response204
  , Response400
  , Response401
  , Response403
  , Response404
  , Response409
  , Response422
  , Response500
  , Response503
  , return200
  , return201
  , return201WithHeaders
  , return204
  , return204WithHeaders
  , return400
  , return401
  , return403
  , return404
  , return409
  , return422
  , return500
  , return503
  )
where

import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Fleece.Aeson.Encoder (encode)
import Fleece.Core qualified as FC
import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTP
import Shrubbery (type (@=))
import Shrubbery qualified as S

import Orb.Response.Document (Document (..))
import Orb.Response.Response (ResponseData (..), ResponseSchema (..), ResponseSchemasBuilder (..))
import Orb.Response.Schemas (NoContent)

addResponseSchema ::
  forall tag tags a.
  KnownHTTPStatus tag =>
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder ((tag @= (a, HTTP.ResponseHeaders)) : tags)
addResponseSchema schema builder =
  let
    proxyTag :: Proxy tag
    proxyTag = Proxy

    status =
      httpStatusVal proxyTag

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (value, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataBytes = encode schema value
        , responseDataContentType = Just "application/json"
        , responseDataExtraHeaders = headers
        }
  in
    ResponseSchemasBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert status (ResponseSchema schema) (responseStatusMapBuilder builder)
      }

addResponseDocument ::
  forall tag tags.
  KnownHTTPStatus tag =>
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder ((tag @= (Document, HTTP.ResponseHeaders)) : tags)
addResponseDocument builder =
  let
    proxyTag :: Proxy tag
    proxyTag = Proxy

    status =
      httpStatusVal proxyTag

    encodeDocument (document, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataBytes = documentContent document
        , responseDataContentType = Just $ documentType document
        , responseDataExtraHeaders =
            ("Content-Disposition", "attachment;filename=" <> documentFileName document)
              : headers
        }
  in
    ResponseSchemasBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag encodeDocument (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert status ResponseDocument (responseStatusMapBuilder builder)
      }

addNoResponseSchema ::
  forall tag tags.
  KnownHTTPStatus tag =>
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder ((tag @= (NoContent, HTTP.ResponseHeaders)) : tags)
addNoResponseSchema builder =
  let
    proxyTag :: Proxy tag
    proxyTag = Proxy

    status =
      httpStatusVal proxyTag

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (_, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataBytes = mempty
        , responseDataContentType = Nothing
        , responseDataExtraHeaders = headers
        }
  in
    ResponseSchemasBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert status NoResponseSchema (responseStatusMapBuilder builder)
      }

class KnownHTTPStatus tag where
  httpStatusVal :: proxy tag -> HTTP.Status

type Response200 a = "200" @= (a, HTTP.ResponseHeaders)
type Response201 a = "201" @= (a, HTTP.ResponseHeaders)
type Response204 = "204" @= (NoContent, HTTP.ResponseHeaders)
type Response400 a = "400" @= (a, HTTP.ResponseHeaders)
type Response401 a = "401" @= (a, HTTP.ResponseHeaders)
type Response403 a = "403" @= (a, HTTP.ResponseHeaders)
type Response404 a = "404" @= (a, HTTP.ResponseHeaders)
type Response409 a = "409" @= (a, HTTP.ResponseHeaders)
type Response422 a = "422" @= (a, HTTP.ResponseHeaders)
type Response500 a = "500" @= (a, HTTP.ResponseHeaders)
type Response503 a = "503" @= (a, HTTP.ResponseHeaders)

addResponseSchema200 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response200 a : tags)
addResponseSchema200 =
  addResponseSchema @"200"

addResponseDocument200 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response200 Document : tags)
addResponseDocument200 =
  addResponseDocument @"200"

addResponseSchema201 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response201 a : tags)
addResponseSchema201 =
  addResponseSchema @"201"

addResponseSchema204 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response204 : tags)
addResponseSchema204 =
  addNoResponseSchema @"204"

addResponseSchema400 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response400 a : tags)
addResponseSchema400 =
  addResponseSchema @"400"

addResponseSchema401 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response401 a : tags)
addResponseSchema401 =
  addResponseSchema @"401"

addResponseSchema403 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response403 a : tags)
addResponseSchema403 =
  addResponseSchema @"403"

addResponseSchema404 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response404 a : tags)
addResponseSchema404 =
  addResponseSchema @"404"

addResponseSchema409 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response409 a : tags)
addResponseSchema409 =
  addResponseSchema @"409"

addResponseSchema422 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response422 a : tags)
addResponseSchema422 =
  addResponseSchema @"422"

addResponseSchema500 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response500 a : tags)
addResponseSchema500 =
  addResponseSchema @"500"

addResponseSchema503 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response503 a : tags)
addResponseSchema503 =
  addResponseSchema @"503"

instance KnownHTTPStatus "200" where
  httpStatusVal _ = HTTP.status200

instance KnownHTTPStatus "201" where
  httpStatusVal _ = HTTP.status201

instance KnownHTTPStatus "204" where
  httpStatusVal _ = HTTP.status204

instance KnownHTTPStatus "400" where
  httpStatusVal _ = HTTP.status400

instance KnownHTTPStatus "401" where
  httpStatusVal _ = HTTP.status401

instance KnownHTTPStatus "403" where
  httpStatusVal _ = HTTP.status403

instance KnownHTTPStatus "404" where
  httpStatusVal _ = HTTP.status404

instance KnownHTTPStatus "409" where
  httpStatusVal _ = HTTP.status409

instance KnownHTTPStatus "422" where
  httpStatusVal _ = HTTP.status422

instance KnownHTTPStatus "500" where
  httpStatusVal _ = HTTP.status500

instance KnownHTTPStatus "503" where
  httpStatusVal _ = HTTP.status503

type ReturnType a n m tags code =
  ( (a, HTTP.ResponseHeaders) ~ S.TagType code tags
  , n ~ S.TagIndex code tags
  , (a, HTTP.ResponseHeaders) ~ S.TypeAtIndex n (S.TaggedTypes tags)
  , KnownNat n
  , Applicative m
  ) =>
  a ->
  m (S.TaggedUnion tags)

type Return200 a n m tags = ReturnType a n m tags "200"
type Return201 a n m tags = ReturnType a n m tags "201"
type Return204 n m tags = ReturnType NoContent n m tags "204"
type Return400 a n m tags = ReturnType a n m tags "400"
type Return401 a n m tags = ReturnType a n m tags "401"
type Return403 a n m tags = ReturnType a n m tags "403"
type Return404 a n m tags = ReturnType a n m tags "404"
type Return409 a n m tags = ReturnType a n m tags "409"
type Return422 a n m tags = ReturnType a n m tags "422"
type Return500 a n m tags = ReturnType a n m tags "500"
type Return503 a n m tags = ReturnType a n m tags "503"

return200 :: Return200 a n m tags
return200 = pure . S.unifyTaggedUnion @"200" . (,[])

return201 :: Return201 a n m tags
return201 = return201WithHeaders []

return201WithHeaders :: HTTP.ResponseHeaders -> Return201 a n m tags
return201WithHeaders headers =
  pure . S.unifyTaggedUnion @"201" . (,headers)

return204 :: Return204 n m tags
return204 = return204WithHeaders []

return204WithHeaders :: HTTP.ResponseHeaders -> Return204 n m tags
return204WithHeaders headers =
  pure . S.unifyTaggedUnion @"204" . (,headers)

return400 :: Return400 a n m tags
return400 = pure . S.unifyTaggedUnion @"400" . (,[])

return401 :: Return401 a n m tags
return401 = pure . S.unifyTaggedUnion @"401" . (,[])

return403 :: Return403 a n m tags
return403 = pure . S.unifyTaggedUnion @"403" . (,[])

return404 :: Return404 a n m tags
return404 = pure . S.unifyTaggedUnion @"404" . (,[])

return409 :: Return409 a n m tags
return409 = pure . S.unifyTaggedUnion @"409" . (,[])

return422 :: Return422 a n m tags
return422 = pure . S.unifyTaggedUnion @"422" . (,[])

return500 :: Return500 a n m tags
return500 = pure . S.unifyTaggedUnion @"500" . (,[])

return503 :: Return503 a n m tags
return503 = pure . S.unifyTaggedUnion @"503" . (,[])
