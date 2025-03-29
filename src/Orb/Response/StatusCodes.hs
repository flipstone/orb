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
  ( addResponseSchema100
  , addResponseSchema101
  , addResponseDocument200
  , addResponseSchema200
  , addResponseDocument201
  , addResponseSchema201
  , addResponseDocument202
  , addResponseSchema202
  , addResponseDocument203
  , addResponseSchema203
  , addResponseSchema204
  , addResponseSchema205
  , addResponseDocument206
  , addResponseSchema206
  , addResponseDocument300
  , addResponseSchema300
  , addResponseDocument301
  , addResponseSchema301
  , addResponseDocument302
  , addResponseSchema302
  , addResponseDocument303
  , addResponseSchema303
  , addResponseSchema304
  , addResponseDocument305
  , addResponseSchema305
  , addResponseDocument307
  , addResponseSchema307
  , addResponseDocument308
  , addResponseSchema308
  , addResponseDocument400
  , addResponseSchema400
  , addResponseDocument401
  , addResponseSchema401
  , addResponseDocument402
  , addResponseSchema402
  , addResponseDocument403
  , addResponseSchema403
  , addResponseDocument404
  , addResponseSchema404
  , addResponseDocument405
  , addResponseSchema405
  , addResponseDocument406
  , addResponseSchema406
  , addResponseDocument407
  , addResponseSchema407
  , addResponseDocument408
  , addResponseSchema408
  , addResponseDocument409
  , addResponseSchema409
  , addResponseDocument410
  , addResponseSchema410
  , addResponseDocument411
  , addResponseSchema411
  , addResponseDocument412
  , addResponseSchema412
  , addResponseDocument413
  , addResponseSchema413
  , addResponseDocument414
  , addResponseSchema414
  , addResponseDocument415
  , addResponseSchema415
  , addResponseDocument416
  , addResponseSchema416
  , addResponseDocument417
  , addResponseSchema417
  , addResponseDocument418
  , addResponseSchema418
  , addResponseDocument422
  , addResponseSchema422
  , addResponseDocument428
  , addResponseSchema428
  , addResponseDocument429
  , addResponseSchema429
  , addResponseDocument431
  , addResponseSchema431
  , addResponseDocument500
  , addResponseSchema500
  , addResponseDocument501
  , addResponseSchema501
  , addResponseDocument502
  , addResponseSchema502
  , addResponseDocument503
  , addResponseSchema503
  , addResponseDocument504
  , addResponseSchema504
  , addResponseDocument505
  , addResponseSchema505
  , addResponseDocument511
  , addResponseSchema511
  , Response100
  , Response101
  , Response200
  , Response201
  , Response202
  , Response203
  , Response204
  , Response205
  , Response206
  , Response300
  , Response301
  , Response302
  , Response303
  , Response304
  , Response305
  , Response307
  , Response308
  , Response400
  , Response401
  , Response402
  , Response403
  , Response404
  , Response405
  , Response406
  , Response407
  , Response408
  , Response409
  , Response410
  , Response411
  , Response412
  , Response413
  , Response414
  , Response415
  , Response416
  , Response417
  , Response418
  , Response422
  , Response428
  , Response429
  , Response431
  , Response500
  , Response501
  , Response502
  , Response503
  , Response504
  , Response505
  , Response511
  , return100
  , return100WithHeaders
  , return101
  , return101WithHeaders
  , return200
  , return200WithHeaders
  , return201
  , return201WithHeaders
  , return202
  , return202WithHeaders
  , return203
  , return203WithHeaders
  , return204
  , return204WithHeaders
  , return205
  , return205WithHeaders
  , return206
  , return206WithHeaders
  , return300
  , return300WithHeaders
  , return301
  , return301WithHeaders
  , return302
  , return302WithHeaders
  , return303
  , return303WithHeaders
  , return304
  , return304WithHeaders
  , return305
  , return305WithHeaders
  , return307
  , return307WithHeaders
  , return308
  , return308WithHeaders
  , return400
  , return400WithHeaders
  , return401
  , return401WithHeaders
  , return402
  , return402WithHeaders
  , return403
  , return403WithHeaders
  , return404
  , return404WithHeaders
  , return405
  , return405WithHeaders
  , return406
  , return406WithHeaders
  , return407
  , return407WithHeaders
  , return408
  , return408WithHeaders
  , return409
  , return409WithHeaders
  , return410
  , return410WithHeaders
  , return411
  , return411WithHeaders
  , return412
  , return412WithHeaders
  , return413
  , return413WithHeaders
  , return414
  , return414WithHeaders
  , return415
  , return415WithHeaders
  , return416
  , return416WithHeaders
  , return417
  , return417WithHeaders
  , return418
  , return418WithHeaders
  , return422
  , return422WithHeaders
  , return428
  , return428WithHeaders
  , return429
  , return429WithHeaders
  , return431
  , return431WithHeaders
  , return500
  , return500WithHeaders
  , return501
  , return501WithHeaders
  , return502
  , return502WithHeaders
  , return503
  , return503WithHeaders
  , return504
  , return504WithHeaders
  , return505
  , return505WithHeaders
  , return511
  , return511WithHeaders
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
import Orb.Response.Schemas (NoContent (..))

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

type Response100 = "100" @= (NoContent, HTTP.ResponseHeaders)
type Response101 = "101" @= (NoContent, HTTP.ResponseHeaders)
type Response200 a = "200" @= (a, HTTP.ResponseHeaders)
type Response201 a = "201" @= (a, HTTP.ResponseHeaders)
type Response202 a = "202" @= (a, HTTP.ResponseHeaders)
type Response203 a = "203" @= (a, HTTP.ResponseHeaders)
type Response204 = "204" @= (NoContent, HTTP.ResponseHeaders)
type Response205 = "205" @= (NoContent, HTTP.ResponseHeaders)
type Response206 a = "206" @= (a, HTTP.ResponseHeaders)
type Response300 a = "300" @= (a, HTTP.ResponseHeaders)
type Response301 a = "301" @= (a, HTTP.ResponseHeaders)
type Response302 a = "302" @= (a, HTTP.ResponseHeaders)
type Response303 a = "303" @= (a, HTTP.ResponseHeaders)
type Response304 = "304" @= (NoContent, HTTP.ResponseHeaders)
type Response305 a = "305" @= (a, HTTP.ResponseHeaders)
type Response307 a = "307" @= (a, HTTP.ResponseHeaders)
type Response308 a = "308" @= (a, HTTP.ResponseHeaders)
type Response400 a = "400" @= (a, HTTP.ResponseHeaders)
type Response401 a = "401" @= (a, HTTP.ResponseHeaders)
type Response402 a = "402" @= (a, HTTP.ResponseHeaders)
type Response403 a = "403" @= (a, HTTP.ResponseHeaders)
type Response404 a = "404" @= (a, HTTP.ResponseHeaders)
type Response405 a = "405" @= (a, HTTP.ResponseHeaders)
type Response406 a = "406" @= (a, HTTP.ResponseHeaders)
type Response407 a = "407" @= (a, HTTP.ResponseHeaders)
type Response408 a = "408" @= (a, HTTP.ResponseHeaders)
type Response409 a = "409" @= (a, HTTP.ResponseHeaders)
type Response410 a = "410" @= (a, HTTP.ResponseHeaders)
type Response411 a = "411" @= (a, HTTP.ResponseHeaders)
type Response412 a = "412" @= (a, HTTP.ResponseHeaders)
type Response413 a = "413" @= (a, HTTP.ResponseHeaders)
type Response414 a = "414" @= (a, HTTP.ResponseHeaders)
type Response415 a = "415" @= (a, HTTP.ResponseHeaders)
type Response416 a = "416" @= (a, HTTP.ResponseHeaders)
type Response417 a = "417" @= (a, HTTP.ResponseHeaders)
type Response418 a = "418" @= (a, HTTP.ResponseHeaders)
type Response422 a = "422" @= (a, HTTP.ResponseHeaders)
type Response428 a = "428" @= (a, HTTP.ResponseHeaders)
type Response429 a = "429" @= (a, HTTP.ResponseHeaders)
type Response431 a = "431" @= (a, HTTP.ResponseHeaders)
type Response500 a = "500" @= (a, HTTP.ResponseHeaders)
type Response501 a = "501" @= (a, HTTP.ResponseHeaders)
type Response502 a = "502" @= (a, HTTP.ResponseHeaders)
type Response503 a = "503" @= (a, HTTP.ResponseHeaders)
type Response504 a = "504" @= (a, HTTP.ResponseHeaders)
type Response505 a = "505" @= (a, HTTP.ResponseHeaders)
type Response511 a = "511" @= (a, HTTP.ResponseHeaders)

addResponseSchema100 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response100 : tags)
addResponseSchema100 =
  addNoResponseSchema @"100"

addResponseSchema101 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response101 : tags)
addResponseSchema101 =
  addNoResponseSchema @"101"

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

addResponseDocument201 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response201 Document : tags)
addResponseDocument201 =
  addResponseDocument @"201"

addResponseSchema202 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response202 a : tags)
addResponseSchema202 =
  addResponseSchema @"202"

addResponseDocument202 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response202 Document : tags)
addResponseDocument202 =
  addResponseDocument @"202"

addResponseSchema203 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response203 a : tags)
addResponseSchema203 =
  addResponseSchema @"203"

addResponseDocument203 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response203 Document : tags)
addResponseDocument203 =
  addResponseDocument @"203"

addResponseSchema204 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response204 : tags)
addResponseSchema204 =
  addNoResponseSchema @"204"

addResponseSchema205 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response205 : tags)
addResponseSchema205 =
  addNoResponseSchema @"205"

addResponseSchema206 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response206 a : tags)
addResponseSchema206 =
  addResponseSchema @"206"

addResponseDocument206 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response206 Document : tags)
addResponseDocument206 =
  addResponseDocument @"206"

addResponseSchema300 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response300 a : tags)
addResponseSchema300 =
  addResponseSchema @"300"

addResponseDocument300 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response300 Document : tags)
addResponseDocument300 =
  addResponseDocument @"300"

addResponseSchema301 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response301 a : tags)
addResponseSchema301 =
  addResponseSchema @"301"

addResponseDocument301 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response301 Document : tags)
addResponseDocument301 =
  addResponseDocument @"301"

addResponseSchema302 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response302 a : tags)
addResponseSchema302 =
  addResponseSchema @"302"

addResponseDocument302 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response302 Document : tags)
addResponseDocument302 =
  addResponseDocument @"302"

addResponseSchema303 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response303 a : tags)
addResponseSchema303 =
  addResponseSchema @"303"

addResponseDocument303 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response303 Document : tags)
addResponseDocument303 =
  addResponseDocument @"303"

addResponseSchema304 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response304 : tags)
addResponseSchema304 =
  addNoResponseSchema @"304"

addResponseSchema305 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response305 a : tags)
addResponseSchema305 =
  addResponseSchema @"305"

addResponseDocument305 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response305 Document : tags)
addResponseDocument305 =
  addResponseDocument @"305"

addResponseSchema307 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response307 a : tags)
addResponseSchema307 =
  addResponseSchema @"307"

addResponseDocument307 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response307 Document : tags)
addResponseDocument307 =
  addResponseDocument @"307"

addResponseSchema308 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response308 a : tags)
addResponseSchema308 =
  addResponseSchema @"308"

addResponseDocument308 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response308 Document : tags)
addResponseDocument308 =
  addResponseDocument @"308"

addResponseSchema400 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response400 a : tags)
addResponseSchema400 =
  addResponseSchema @"400"

addResponseDocument400 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response400 Document : tags)
addResponseDocument400 =
  addResponseDocument @"400"

addResponseSchema401 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response401 a : tags)
addResponseSchema401 =
  addResponseSchema @"401"

addResponseDocument401 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response401 Document : tags)
addResponseDocument401 =
  addResponseDocument @"401"

addResponseSchema402 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response402 a : tags)
addResponseSchema402 =
  addResponseSchema @"402"

addResponseDocument402 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response402 Document : tags)
addResponseDocument402 =
  addResponseDocument @"402"

addResponseSchema403 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response403 a : tags)
addResponseSchema403 =
  addResponseSchema @"403"

addResponseDocument403 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response403 Document : tags)
addResponseDocument403 =
  addResponseDocument @"403"

addResponseSchema404 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response404 a : tags)
addResponseSchema404 =
  addResponseSchema @"404"

addResponseDocument404 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response404 Document : tags)
addResponseDocument404 =
  addResponseDocument @"404"

addResponseSchema405 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response405 a : tags)
addResponseSchema405 =
  addResponseSchema @"405"

addResponseDocument405 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response405 Document : tags)
addResponseDocument405 =
  addResponseDocument @"405"

addResponseSchema406 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response406 a : tags)
addResponseSchema406 =
  addResponseSchema @"406"

addResponseDocument406 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response406 Document : tags)
addResponseDocument406 =
  addResponseDocument @"406"

addResponseSchema407 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response407 a : tags)
addResponseSchema407 =
  addResponseSchema @"407"

addResponseDocument407 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response407 Document : tags)
addResponseDocument407 =
  addResponseDocument @"407"

addResponseSchema408 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response408 a : tags)
addResponseSchema408 =
  addResponseSchema @"408"

addResponseDocument408 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response408 Document : tags)
addResponseDocument408 =
  addResponseDocument @"408"

addResponseSchema409 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response409 a : tags)
addResponseSchema409 =
  addResponseSchema @"409"

addResponseDocument409 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response409 Document : tags)
addResponseDocument409 =
  addResponseDocument @"409"

addResponseSchema410 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response410 a : tags)
addResponseSchema410 =
  addResponseSchema @"410"

addResponseDocument410 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response410 Document : tags)
addResponseDocument410 =
  addResponseDocument @"410"

addResponseSchema411 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response411 a : tags)
addResponseSchema411 =
  addResponseSchema @"411"

addResponseDocument411 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response411 Document : tags)
addResponseDocument411 =
  addResponseDocument @"411"

addResponseSchema412 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response412 a : tags)
addResponseSchema412 =
  addResponseSchema @"412"

addResponseDocument412 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response412 Document : tags)
addResponseDocument412 =
  addResponseDocument @"412"

addResponseSchema413 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response413 a : tags)
addResponseSchema413 =
  addResponseSchema @"413"

addResponseDocument413 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response413 Document : tags)
addResponseDocument413 =
  addResponseDocument @"413"

addResponseSchema414 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response414 a : tags)
addResponseSchema414 =
  addResponseSchema @"414"

addResponseDocument414 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response414 Document : tags)
addResponseDocument414 =
  addResponseDocument @"414"

addResponseSchema415 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response415 a : tags)
addResponseSchema415 =
  addResponseSchema @"415"

addResponseDocument415 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response415 Document : tags)
addResponseDocument415 =
  addResponseDocument @"415"

addResponseSchema416 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response416 a : tags)
addResponseSchema416 =
  addResponseSchema @"416"

addResponseDocument416 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response416 Document : tags)
addResponseDocument416 =
  addResponseDocument @"416"

addResponseSchema417 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response417 a : tags)
addResponseSchema417 =
  addResponseSchema @"417"

addResponseDocument417 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response417 Document : tags)
addResponseDocument417 =
  addResponseDocument @"417"

addResponseSchema418 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response418 a : tags)
addResponseSchema418 =
  addResponseSchema @"418"

addResponseDocument418 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response418 Document : tags)
addResponseDocument418 =
  addResponseDocument @"418"

addResponseSchema422 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response422 a : tags)
addResponseSchema422 =
  addResponseSchema @"422"

addResponseDocument422 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response422 Document : tags)
addResponseDocument422 =
  addResponseDocument @"422"

addResponseSchema428 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response428 a : tags)
addResponseSchema428 =
  addResponseSchema @"428"

addResponseDocument428 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response428 Document : tags)
addResponseDocument428 =
  addResponseDocument @"428"

addResponseSchema429 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response429 a : tags)
addResponseSchema429 =
  addResponseSchema @"429"

addResponseDocument429 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response429 Document : tags)
addResponseDocument429 =
  addResponseDocument @"429"

addResponseSchema431 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response431 a : tags)
addResponseSchema431 =
  addResponseSchema @"431"

addResponseDocument431 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response431 Document : tags)
addResponseDocument431 =
  addResponseDocument @"431"

addResponseSchema500 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response500 a : tags)
addResponseSchema500 =
  addResponseSchema @"500"

addResponseDocument500 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response500 Document : tags)
addResponseDocument500 =
  addResponseDocument @"500"

addResponseSchema501 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response501 a : tags)
addResponseSchema501 =
  addResponseSchema @"501"

addResponseDocument501 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response501 Document : tags)
addResponseDocument501 =
  addResponseDocument @"501"

addResponseSchema502 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response502 a : tags)
addResponseSchema502 =
  addResponseSchema @"502"

addResponseDocument502 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response502 Document : tags)
addResponseDocument502 =
  addResponseDocument @"502"

addResponseSchema503 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response503 a : tags)
addResponseSchema503 =
  addResponseSchema @"503"

addResponseDocument503 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response503 Document : tags)
addResponseDocument503 =
  addResponseDocument @"503"

addResponseSchema504 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response504 a : tags)
addResponseSchema504 =
  addResponseSchema @"504"

addResponseDocument504 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response504 Document : tags)
addResponseDocument504 =
  addResponseDocument @"504"

addResponseSchema505 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response505 a : tags)
addResponseSchema505 =
  addResponseSchema @"505"

addResponseDocument505 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response505 Document : tags)
addResponseDocument505 =
  addResponseDocument @"505"

addResponseSchema511 ::
  (forall schema. FC.Fleece schema => schema a) ->
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response511 a : tags)
addResponseSchema511 =
  addResponseSchema @"511"

addResponseDocument511 ::
  ResponseSchemasBuilder tags ->
  ResponseSchemasBuilder (Response511 Document : tags)
addResponseDocument511 =
  addResponseDocument @"511"

instance KnownHTTPStatus "100" where
  httpStatusVal _ = HTTP.status100

instance KnownHTTPStatus "101" where
  httpStatusVal _ = HTTP.status101

instance KnownHTTPStatus "200" where
  httpStatusVal _ = HTTP.status200

instance KnownHTTPStatus "201" where
  httpStatusVal _ = HTTP.status201

instance KnownHTTPStatus "202" where
  httpStatusVal _ = HTTP.status202

instance KnownHTTPStatus "203" where
  httpStatusVal _ = HTTP.status203

instance KnownHTTPStatus "204" where
  httpStatusVal _ = HTTP.status204

instance KnownHTTPStatus "205" where
  httpStatusVal _ = HTTP.status205

instance KnownHTTPStatus "206" where
  httpStatusVal _ = HTTP.status206

instance KnownHTTPStatus "300" where
  httpStatusVal _ = HTTP.status300

instance KnownHTTPStatus "301" where
  httpStatusVal _ = HTTP.status301

instance KnownHTTPStatus "302" where
  httpStatusVal _ = HTTP.status302

instance KnownHTTPStatus "303" where
  httpStatusVal _ = HTTP.status303

instance KnownHTTPStatus "304" where
  httpStatusVal _ = HTTP.status304

instance KnownHTTPStatus "305" where
  httpStatusVal _ = HTTP.status305

instance KnownHTTPStatus "307" where
  httpStatusVal _ = HTTP.status307

instance KnownHTTPStatus "308" where
  httpStatusVal _ = HTTP.status308

instance KnownHTTPStatus "400" where
  httpStatusVal _ = HTTP.status400

instance KnownHTTPStatus "401" where
  httpStatusVal _ = HTTP.status401

instance KnownHTTPStatus "402" where
  httpStatusVal _ = HTTP.status402

instance KnownHTTPStatus "403" where
  httpStatusVal _ = HTTP.status403

instance KnownHTTPStatus "404" where
  httpStatusVal _ = HTTP.status404

instance KnownHTTPStatus "405" where
  httpStatusVal _ = HTTP.status405

instance KnownHTTPStatus "406" where
  httpStatusVal _ = HTTP.status406

instance KnownHTTPStatus "407" where
  httpStatusVal _ = HTTP.status407

instance KnownHTTPStatus "408" where
  httpStatusVal _ = HTTP.status408

instance KnownHTTPStatus "409" where
  httpStatusVal _ = HTTP.status409

instance KnownHTTPStatus "410" where
  httpStatusVal _ = HTTP.status410

instance KnownHTTPStatus "411" where
  httpStatusVal _ = HTTP.status411

instance KnownHTTPStatus "412" where
  httpStatusVal _ = HTTP.status412

instance KnownHTTPStatus "413" where
  httpStatusVal _ = HTTP.status413

instance KnownHTTPStatus "414" where
  httpStatusVal _ = HTTP.status414

instance KnownHTTPStatus "415" where
  httpStatusVal _ = HTTP.status415

instance KnownHTTPStatus "416" where
  httpStatusVal _ = HTTP.status416

instance KnownHTTPStatus "417" where
  httpStatusVal _ = HTTP.status417

instance KnownHTTPStatus "418" where
  httpStatusVal _ = HTTP.status418

instance KnownHTTPStatus "422" where
  httpStatusVal _ = HTTP.status422

instance KnownHTTPStatus "428" where
  httpStatusVal _ = HTTP.status428

instance KnownHTTPStatus "429" where
  httpStatusVal _ = HTTP.status429

instance KnownHTTPStatus "431" where
  httpStatusVal _ = HTTP.status431

instance KnownHTTPStatus "500" where
  httpStatusVal _ = HTTP.status500

instance KnownHTTPStatus "501" where
  httpStatusVal _ = HTTP.status501

instance KnownHTTPStatus "502" where
  httpStatusVal _ = HTTP.status502

instance KnownHTTPStatus "503" where
  httpStatusVal _ = HTTP.status503

instance KnownHTTPStatus "504" where
  httpStatusVal _ = HTTP.status504

instance KnownHTTPStatus "505" where
  httpStatusVal _ = HTTP.status505

instance KnownHTTPStatus "511" where
  httpStatusVal _ = HTTP.status511

type ReturnType a n m tags code =
  ( (a, HTTP.ResponseHeaders) ~ S.TagType code tags
  , n ~ S.TagIndex code tags
  , (a, HTTP.ResponseHeaders) ~ S.TypeAtIndex n (S.TaggedTypes tags)
  , KnownNat n
  , Applicative m
  ) =>
  a ->
  m (S.TaggedUnion tags)

type Return100 n m tags = ReturnType NoContent n m tags "100"
type Return101 n m tags = ReturnType NoContent n m tags "101"
type Return200 a n m tags = ReturnType a n m tags "200"
type Return201 a n m tags = ReturnType a n m tags "201"
type Return202 a n m tags = ReturnType a n m tags "202"
type Return203 a n m tags = ReturnType a n m tags "203"
type Return204 n m tags = ReturnType NoContent n m tags "204"
type Return205 n m tags = ReturnType NoContent n m tags "205"
type Return206 a n m tags = ReturnType a n m tags "206"
type Return300 a n m tags = ReturnType a n m tags "300"
type Return301 a n m tags = ReturnType a n m tags "301"
type Return302 a n m tags = ReturnType a n m tags "302"
type Return303 a n m tags = ReturnType a n m tags "303"
type Return304 n m tags = ReturnType NoContent n m tags "304"
type Return305 a n m tags = ReturnType a n m tags "305"
type Return307 a n m tags = ReturnType a n m tags "307"
type Return308 a n m tags = ReturnType a n m tags "308"
type Return400 a n m tags = ReturnType a n m tags "400"
type Return401 a n m tags = ReturnType a n m tags "401"
type Return402 a n m tags = ReturnType a n m tags "402"
type Return403 a n m tags = ReturnType a n m tags "403"
type Return404 a n m tags = ReturnType a n m tags "404"
type Return405 a n m tags = ReturnType a n m tags "405"
type Return406 a n m tags = ReturnType a n m tags "406"
type Return407 a n m tags = ReturnType a n m tags "407"
type Return408 a n m tags = ReturnType a n m tags "408"
type Return409 a n m tags = ReturnType a n m tags "409"
type Return410 a n m tags = ReturnType a n m tags "410"
type Return411 a n m tags = ReturnType a n m tags "411"
type Return412 a n m tags = ReturnType a n m tags "412"
type Return413 a n m tags = ReturnType a n m tags "413"
type Return414 a n m tags = ReturnType a n m tags "414"
type Return415 a n m tags = ReturnType a n m tags "415"
type Return416 a n m tags = ReturnType a n m tags "416"
type Return417 a n m tags = ReturnType a n m tags "417"
type Return418 a n m tags = ReturnType a n m tags "418"
type Return422 a n m tags = ReturnType a n m tags "422"
type Return428 a n m tags = ReturnType a n m tags "428"
type Return429 a n m tags = ReturnType a n m tags "429"
type Return431 a n m tags = ReturnType a n m tags "431"
type Return500 a n m tags = ReturnType a n m tags "500"
type Return501 a n m tags = ReturnType a n m tags "501"
type Return502 a n m tags = ReturnType a n m tags "502"
type Return503 a n m tags = ReturnType a n m tags "503"
type Return504 a n m tags = ReturnType a n m tags "504"
type Return505 a n m tags = ReturnType a n m tags "505"
type Return511 a n m tags = ReturnType a n m tags "511"

return100 :: Return100 n m tags
return100 = return100WithHeaders []

return100WithHeaders :: HTTP.ResponseHeaders -> Return100 n m tags
return100WithHeaders headers =
  pure . S.unifyTaggedUnion @"100" . (,headers)

return101 :: Return101 n m tags
return101 = return101WithHeaders []

return101WithHeaders :: HTTP.ResponseHeaders -> Return101 n m tags
return101WithHeaders headers =
  pure . S.unifyTaggedUnion @"101" . (,headers)

return200 :: Return200 a n m tags
return200 = return200WithHeaders []

return200WithHeaders :: HTTP.ResponseHeaders -> Return200 a n m tags
return200WithHeaders headers =
  pure . S.unifyTaggedUnion @"200" . (,headers)

return201 :: Return201 a n m tags
return201 = return201WithHeaders []

return201WithHeaders :: HTTP.ResponseHeaders -> Return201 a n m tags
return201WithHeaders headers =
  pure . S.unifyTaggedUnion @"201" . (,headers)

return202 :: Return202 a n m tags
return202 = return202WithHeaders []

return202WithHeaders :: HTTP.ResponseHeaders -> Return202 a n m tags
return202WithHeaders headers =
  pure . S.unifyTaggedUnion @"202" . (,headers)

return203 :: Return203 a n m tags
return203 = return203WithHeaders []

return203WithHeaders :: HTTP.ResponseHeaders -> Return203 a n m tags
return203WithHeaders headers =
  pure . S.unifyTaggedUnion @"203" . (,headers)

return204 :: Return204 n m tags
return204 = return204WithHeaders []

return204WithHeaders :: HTTP.ResponseHeaders -> Return204 n m tags
return204WithHeaders headers =
  pure . S.unifyTaggedUnion @"204" . (,headers)

return205 :: Return205 n m tags
return205 = return205WithHeaders []

return205WithHeaders :: HTTP.ResponseHeaders -> Return205 n m tags
return205WithHeaders headers =
  pure . S.unifyTaggedUnion @"205" . (,headers)

return206 :: Return206 a n m tags
return206 = return206WithHeaders []

return206WithHeaders :: HTTP.ResponseHeaders -> Return206 a n m tags
return206WithHeaders headers =
  pure . S.unifyTaggedUnion @"206" . (,headers)

return300 :: Return300 a n m tags
return300 = return300WithHeaders []

return300WithHeaders :: HTTP.ResponseHeaders -> Return300 a n m tags
return300WithHeaders headers =
  pure . S.unifyTaggedUnion @"300" . (,headers)

return301 :: Return301 a n m tags
return301 = return301WithHeaders []

return301WithHeaders :: HTTP.ResponseHeaders -> Return301 a n m tags
return301WithHeaders headers =
  pure . S.unifyTaggedUnion @"301" . (,headers)

return302 :: Return302 a n m tags
return302 = return302WithHeaders []

return302WithHeaders :: HTTP.ResponseHeaders -> Return302 a n m tags
return302WithHeaders headers =
  pure . S.unifyTaggedUnion @"302" . (,headers)

return303 :: Return303 a n m tags
return303 = return303WithHeaders []

return303WithHeaders :: HTTP.ResponseHeaders -> Return303 a n m tags
return303WithHeaders headers =
  pure . S.unifyTaggedUnion @"303" . (,headers)

return304 :: Return304 n m tags
return304 = return304WithHeaders []

return304WithHeaders :: HTTP.ResponseHeaders -> Return304 n m tags
return304WithHeaders headers =
  pure . S.unifyTaggedUnion @"304" . (,headers)

return305 :: Return305 a n m tags
return305 = return305WithHeaders []

return305WithHeaders :: HTTP.ResponseHeaders -> Return305 a n m tags
return305WithHeaders headers =
  pure . S.unifyTaggedUnion @"305" . (,headers)

return307 :: Return307 a n m tags
return307 = return307WithHeaders []

return307WithHeaders :: HTTP.ResponseHeaders -> Return307 a n m tags
return307WithHeaders headers =
  pure . S.unifyTaggedUnion @"307" . (,headers)

return308 :: Return308 a n m tags
return308 = return308WithHeaders []

return308WithHeaders :: HTTP.ResponseHeaders -> Return308 a n m tags
return308WithHeaders headers =
  pure . S.unifyTaggedUnion @"308" . (,headers)

return400 :: Return400 a n m tags
return400 = return400WithHeaders []

return400WithHeaders :: HTTP.ResponseHeaders -> Return400 a n m tags
return400WithHeaders headers =
  pure . S.unifyTaggedUnion @"400" . (,headers)

return401 :: Return401 a n m tags
return401 = return401WithHeaders []

return401WithHeaders :: HTTP.ResponseHeaders -> Return401 a n m tags
return401WithHeaders headers =
  pure . S.unifyTaggedUnion @"401" . (,headers)

return402 :: Return402 a n m tags
return402 = return402WithHeaders []

return402WithHeaders :: HTTP.ResponseHeaders -> Return402 a n m tags
return402WithHeaders headers =
  pure . S.unifyTaggedUnion @"402" . (,headers)

return403 :: Return403 a n m tags
return403 = return403WithHeaders []

return403WithHeaders :: HTTP.ResponseHeaders -> Return403 a n m tags
return403WithHeaders headers =
  pure . S.unifyTaggedUnion @"403" . (,headers)

return404 :: Return404 a n m tags
return404 = return404WithHeaders []

return404WithHeaders :: HTTP.ResponseHeaders -> Return404 a n m tags
return404WithHeaders headers =
  pure . S.unifyTaggedUnion @"404" . (,headers)

return405 :: Return405 a n m tags
return405 = return405WithHeaders []

return405WithHeaders :: HTTP.ResponseHeaders -> Return405 a n m tags
return405WithHeaders headers =
  pure . S.unifyTaggedUnion @"405" . (,headers)

return406 :: Return406 a n m tags
return406 = return406WithHeaders []

return406WithHeaders :: HTTP.ResponseHeaders -> Return406 a n m tags
return406WithHeaders headers =
  pure . S.unifyTaggedUnion @"406" . (,headers)

return407 :: Return407 a n m tags
return407 = return407WithHeaders []

return407WithHeaders :: HTTP.ResponseHeaders -> Return407 a n m tags
return407WithHeaders headers =
  pure . S.unifyTaggedUnion @"407" . (,headers)

return408 :: Return408 a n m tags
return408 = return408WithHeaders []

return408WithHeaders :: HTTP.ResponseHeaders -> Return408 a n m tags
return408WithHeaders headers =
  pure . S.unifyTaggedUnion @"408" . (,headers)

return409 :: Return409 a n m tags
return409 = return409WithHeaders []

return409WithHeaders :: HTTP.ResponseHeaders -> Return409 a n m tags
return409WithHeaders headers =
  pure . S.unifyTaggedUnion @"409" . (,headers)

return410 :: Return410 a n m tags
return410 = return410WithHeaders []

return410WithHeaders :: HTTP.ResponseHeaders -> Return410 a n m tags
return410WithHeaders headers =
  pure . S.unifyTaggedUnion @"410" . (,headers)

return411 :: Return411 a n m tags
return411 = return411WithHeaders []

return411WithHeaders :: HTTP.ResponseHeaders -> Return411 a n m tags
return411WithHeaders headers =
  pure . S.unifyTaggedUnion @"411" . (,headers)

return412 :: Return412 a n m tags
return412 = return412WithHeaders []

return412WithHeaders :: HTTP.ResponseHeaders -> Return412 a n m tags
return412WithHeaders headers =
  pure . S.unifyTaggedUnion @"412" . (,headers)

return413 :: Return413 a n m tags
return413 = return413WithHeaders []

return413WithHeaders :: HTTP.ResponseHeaders -> Return413 a n m tags
return413WithHeaders headers =
  pure . S.unifyTaggedUnion @"413" . (,headers)

return414 :: Return414 a n m tags
return414 = return414WithHeaders []

return414WithHeaders :: HTTP.ResponseHeaders -> Return414 a n m tags
return414WithHeaders headers =
  pure . S.unifyTaggedUnion @"414" . (,headers)

return415 :: Return415 a n m tags
return415 = return415WithHeaders []

return415WithHeaders :: HTTP.ResponseHeaders -> Return415 a n m tags
return415WithHeaders headers =
  pure . S.unifyTaggedUnion @"415" . (,headers)

return416 :: Return416 a n m tags
return416 = return416WithHeaders []

return416WithHeaders :: HTTP.ResponseHeaders -> Return416 a n m tags
return416WithHeaders headers =
  pure . S.unifyTaggedUnion @"416" . (,headers)

return417 :: Return417 a n m tags
return417 = return417WithHeaders []

return417WithHeaders :: HTTP.ResponseHeaders -> Return417 a n m tags
return417WithHeaders headers =
  pure . S.unifyTaggedUnion @"417" . (,headers)

return418 :: Return418 a n m tags
return418 = return418WithHeaders []

return418WithHeaders :: HTTP.ResponseHeaders -> Return418 a n m tags
return418WithHeaders headers =
  pure . S.unifyTaggedUnion @"418" . (,headers)

return422 :: Return422 a n m tags
return422 = return422WithHeaders []

return422WithHeaders :: HTTP.ResponseHeaders -> Return422 a n m tags
return422WithHeaders headers =
  pure . S.unifyTaggedUnion @"422" . (,headers)

return428 :: Return428 a n m tags
return428 = return428WithHeaders []

return428WithHeaders :: HTTP.ResponseHeaders -> Return428 a n m tags
return428WithHeaders headers =
  pure . S.unifyTaggedUnion @"428" . (,headers)

return429 :: Return429 a n m tags
return429 = return429WithHeaders []

return429WithHeaders :: HTTP.ResponseHeaders -> Return429 a n m tags
return429WithHeaders headers =
  pure . S.unifyTaggedUnion @"429" . (,headers)

return431 :: Return431 a n m tags
return431 = return431WithHeaders []

return431WithHeaders :: HTTP.ResponseHeaders -> Return431 a n m tags
return431WithHeaders headers =
  pure . S.unifyTaggedUnion @"431" . (,headers)

return500 :: Return500 a n m tags
return500 = return500WithHeaders []

return500WithHeaders :: HTTP.ResponseHeaders -> Return500 a n m tags
return500WithHeaders headers =
  pure . S.unifyTaggedUnion @"500" . (,headers)

return501 :: Return501 a n m tags
return501 = return501WithHeaders []

return501WithHeaders :: HTTP.ResponseHeaders -> Return501 a n m tags
return501WithHeaders headers =
  pure . S.unifyTaggedUnion @"501" . (,headers)

return502 :: Return502 a n m tags
return502 = return502WithHeaders []

return502WithHeaders :: HTTP.ResponseHeaders -> Return502 a n m tags
return502WithHeaders headers =
  pure . S.unifyTaggedUnion @"502" . (,headers)

return503 :: Return503 a n m tags
return503 = return503WithHeaders []

return503WithHeaders :: HTTP.ResponseHeaders -> Return503 a n m tags
return503WithHeaders headers =
  pure . S.unifyTaggedUnion @"503" . (,headers)

return504 :: Return504 a n m tags
return504 = return504WithHeaders []

return504WithHeaders :: HTTP.ResponseHeaders -> Return504 a n m tags
return504WithHeaders headers =
  pure . S.unifyTaggedUnion @"504" . (,headers)

return505 :: Return505 a n m tags
return505 = return505WithHeaders []

return505WithHeaders :: HTTP.ResponseHeaders -> Return505 a n m tags
return505WithHeaders headers =
  pure . S.unifyTaggedUnion @"505" . (,headers)

return511 :: Return511 a n m tags
return511 = return511WithHeaders []

return511WithHeaders :: HTTP.ResponseHeaders -> Return511 a n m tags
return511WithHeaders headers =
  pure . S.unifyTaggedUnion @"511" . (,headers)
