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
  ( addResponse100
  , addResponse101
  , addResponseDocument200
  , addResponseBody200
  , addResponseDocument201
  , addResponseBody201
  , addResponseDocument202
  , addResponseBody202
  , addResponseDocument203
  , addResponseBody203
  , addResponse204
  , addResponse205
  , addResponseDocument206
  , addResponseBody206
  , addResponseDocument300
  , addResponseBody300
  , addResponseDocument301
  , addResponseBody301
  , addResponseDocument302
  , addResponseBody302
  , addResponseDocument303
  , addResponseBody303
  , addResponse304
  , addResponseDocument305
  , addResponseBody305
  , addResponseDocument307
  , addResponseBody307
  , addResponseDocument308
  , addResponseBody308
  , addResponseDocument400
  , addResponseBody400
  , addResponseDocument401
  , addResponseBody401
  , addResponseDocument402
  , addResponseBody402
  , addResponseDocument403
  , addResponseBody403
  , addResponseDocument404
  , addResponseBody404
  , addResponseDocument405
  , addResponseBody405
  , addResponseDocument406
  , addResponseBody406
  , addResponseDocument407
  , addResponseBody407
  , addResponseDocument408
  , addResponseBody408
  , addResponseDocument409
  , addResponseBody409
  , addResponseDocument410
  , addResponseBody410
  , addResponseDocument411
  , addResponseBody411
  , addResponseDocument412
  , addResponseBody412
  , addResponseDocument413
  , addResponseBody413
  , addResponseDocument414
  , addResponseBody414
  , addResponseDocument415
  , addResponseBody415
  , addResponseDocument416
  , addResponseBody416
  , addResponseDocument417
  , addResponseBody417
  , addResponseDocument418
  , addResponseBody418
  , addResponseDocument422
  , addResponseBody422
  , addResponseDocument428
  , addResponseBody428
  , addResponseDocument429
  , addResponseBody429
  , addResponseDocument431
  , addResponseBody431
  , addResponseDocument500
  , addResponseBody500
  , addResponseDocument501
  , addResponseBody501
  , addResponseDocument502
  , addResponseBody502
  , addResponseDocument503
  , addResponseBody503
  , addResponseDocument504
  , addResponseBody504
  , addResponseDocument505
  , addResponseBody505
  , addResponseDocument511
  , addResponseBody511
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

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTP
import Shrubbery (type (@=))
import Shrubbery qualified as S

import Orb.Response.ContentType (ContentType, contentTypeToBytes)
import Orb.Response.Document (Document (..))
import Orb.Response.Response (ResponseBodiesBuilder (..), ResponseBody (..), ResponseData (..))
import Orb.Response.Schemas (NoContent (..))

addResponseBody ::
  forall tag tags a.
  KnownHTTPStatus tag =>
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder ((tag @= (a, HTTP.ResponseHeaders)) : tags)
addResponseBody contentType encoder builder =
  let
    proxyTag :: Proxy tag
    proxyTag = Proxy

    status =
      httpStatusVal proxyTag

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (value, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataBytes = encoder value
        , responseDataContentType = Just $ contentTypeToBytes contentType
        , responseDataExtraHeaders = headers
        }
  in
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert
            status
            (ResponseContent contentType encoder)
            (responseStatusMapBuilder builder)
      }

addResponseDocument ::
  forall tag tags.
  KnownHTTPStatus tag =>
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder ((tag @= (Document, HTTP.ResponseHeaders)) : tags)
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
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag encodeDocument (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert status ResponseDocument (responseStatusMapBuilder builder)
      }

addNoResponseBody ::
  forall tag tags.
  KnownHTTPStatus tag =>
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder ((tag @= (NoContent, HTTP.ResponseHeaders)) : tags)
addNoResponseBody builder =
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
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @tag runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insert status EmptyResponseBody (responseStatusMapBuilder builder)
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

addResponse100 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response100 : tags)
addResponse100 =
  addNoResponseBody @"100"

addResponse101 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response101 : tags)
addResponse101 =
  addNoResponseBody @"101"

addResponseBody200 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response200 a : tags)
addResponseBody200 =
  addResponseBody @"200"

addResponseDocument200 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response200 Document : tags)
addResponseDocument200 =
  addResponseDocument @"200"

addResponseBody201 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response201 a : tags)
addResponseBody201 =
  addResponseBody @"201"

addResponseDocument201 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response201 Document : tags)
addResponseDocument201 =
  addResponseDocument @"201"

addResponseBody202 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response202 a : tags)
addResponseBody202 =
  addResponseBody @"202"

addResponseDocument202 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response202 Document : tags)
addResponseDocument202 =
  addResponseDocument @"202"

addResponseBody203 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response203 a : tags)
addResponseBody203 =
  addResponseBody @"203"

addResponseDocument203 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response203 Document : tags)
addResponseDocument203 =
  addResponseDocument @"203"

addResponse204 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response204 : tags)
addResponse204 =
  addNoResponseBody @"204"

addResponse205 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response205 : tags)
addResponse205 =
  addNoResponseBody @"205"

addResponseBody206 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response206 a : tags)
addResponseBody206 =
  addResponseBody @"206"

addResponseDocument206 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response206 Document : tags)
addResponseDocument206 =
  addResponseDocument @"206"

addResponseBody300 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response300 a : tags)
addResponseBody300 =
  addResponseBody @"300"

addResponseDocument300 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response300 Document : tags)
addResponseDocument300 =
  addResponseDocument @"300"

addResponseBody301 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response301 a : tags)
addResponseBody301 =
  addResponseBody @"301"

addResponseDocument301 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response301 Document : tags)
addResponseDocument301 =
  addResponseDocument @"301"

addResponseBody302 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response302 a : tags)
addResponseBody302 =
  addResponseBody @"302"

addResponseDocument302 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response302 Document : tags)
addResponseDocument302 =
  addResponseDocument @"302"

addResponseBody303 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response303 a : tags)
addResponseBody303 =
  addResponseBody @"303"

addResponseDocument303 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response303 Document : tags)
addResponseDocument303 =
  addResponseDocument @"303"

addResponse304 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response304 : tags)
addResponse304 =
  addNoResponseBody @"304"

addResponseBody305 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response305 a : tags)
addResponseBody305 =
  addResponseBody @"305"

addResponseDocument305 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response305 Document : tags)
addResponseDocument305 =
  addResponseDocument @"305"

addResponseBody307 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response307 a : tags)
addResponseBody307 =
  addResponseBody @"307"

addResponseDocument307 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response307 Document : tags)
addResponseDocument307 =
  addResponseDocument @"307"

addResponseBody308 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response308 a : tags)
addResponseBody308 =
  addResponseBody @"308"

addResponseDocument308 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response308 Document : tags)
addResponseDocument308 =
  addResponseDocument @"308"

addResponseBody400 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response400 a : tags)
addResponseBody400 =
  addResponseBody @"400"

addResponseDocument400 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response400 Document : tags)
addResponseDocument400 =
  addResponseDocument @"400"

addResponseBody401 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response401 a : tags)
addResponseBody401 =
  addResponseBody @"401"

addResponseDocument401 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response401 Document : tags)
addResponseDocument401 =
  addResponseDocument @"401"

addResponseBody402 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response402 a : tags)
addResponseBody402 =
  addResponseBody @"402"

addResponseDocument402 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response402 Document : tags)
addResponseDocument402 =
  addResponseDocument @"402"

addResponseBody403 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response403 a : tags)
addResponseBody403 =
  addResponseBody @"403"

addResponseDocument403 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response403 Document : tags)
addResponseDocument403 =
  addResponseDocument @"403"

addResponseBody404 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response404 a : tags)
addResponseBody404 =
  addResponseBody @"404"

addResponseDocument404 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response404 Document : tags)
addResponseDocument404 =
  addResponseDocument @"404"

addResponseBody405 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response405 a : tags)
addResponseBody405 =
  addResponseBody @"405"

addResponseDocument405 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response405 Document : tags)
addResponseDocument405 =
  addResponseDocument @"405"

addResponseBody406 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response406 a : tags)
addResponseBody406 =
  addResponseBody @"406"

addResponseDocument406 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response406 Document : tags)
addResponseDocument406 =
  addResponseDocument @"406"

addResponseBody407 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response407 a : tags)
addResponseBody407 =
  addResponseBody @"407"

addResponseDocument407 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response407 Document : tags)
addResponseDocument407 =
  addResponseDocument @"407"

addResponseBody408 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response408 a : tags)
addResponseBody408 =
  addResponseBody @"408"

addResponseDocument408 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response408 Document : tags)
addResponseDocument408 =
  addResponseDocument @"408"

addResponseBody409 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response409 a : tags)
addResponseBody409 =
  addResponseBody @"409"

addResponseDocument409 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response409 Document : tags)
addResponseDocument409 =
  addResponseDocument @"409"

addResponseBody410 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response410 a : tags)
addResponseBody410 =
  addResponseBody @"410"

addResponseDocument410 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response410 Document : tags)
addResponseDocument410 =
  addResponseDocument @"410"

addResponseBody411 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response411 a : tags)
addResponseBody411 =
  addResponseBody @"411"

addResponseDocument411 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response411 Document : tags)
addResponseDocument411 =
  addResponseDocument @"411"

addResponseBody412 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response412 a : tags)
addResponseBody412 =
  addResponseBody @"412"

addResponseDocument412 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response412 Document : tags)
addResponseDocument412 =
  addResponseDocument @"412"

addResponseBody413 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response413 a : tags)
addResponseBody413 =
  addResponseBody @"413"

addResponseDocument413 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response413 Document : tags)
addResponseDocument413 =
  addResponseDocument @"413"

addResponseBody414 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response414 a : tags)
addResponseBody414 =
  addResponseBody @"414"

addResponseDocument414 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response414 Document : tags)
addResponseDocument414 =
  addResponseDocument @"414"

addResponseBody415 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response415 a : tags)
addResponseBody415 =
  addResponseBody @"415"

addResponseDocument415 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response415 Document : tags)
addResponseDocument415 =
  addResponseDocument @"415"

addResponseBody416 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response416 a : tags)
addResponseBody416 =
  addResponseBody @"416"

addResponseDocument416 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response416 Document : tags)
addResponseDocument416 =
  addResponseDocument @"416"

addResponseBody417 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response417 a : tags)
addResponseBody417 =
  addResponseBody @"417"

addResponseDocument417 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response417 Document : tags)
addResponseDocument417 =
  addResponseDocument @"417"

addResponseBody418 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response418 a : tags)
addResponseBody418 =
  addResponseBody @"418"

addResponseDocument418 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response418 Document : tags)
addResponseDocument418 =
  addResponseDocument @"418"

addResponseBody422 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response422 a : tags)
addResponseBody422 =
  addResponseBody @"422"

addResponseDocument422 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response422 Document : tags)
addResponseDocument422 =
  addResponseDocument @"422"

addResponseBody428 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response428 a : tags)
addResponseBody428 =
  addResponseBody @"428"

addResponseDocument428 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response428 Document : tags)
addResponseDocument428 =
  addResponseDocument @"428"

addResponseBody429 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response429 a : tags)
addResponseBody429 =
  addResponseBody @"429"

addResponseDocument429 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response429 Document : tags)
addResponseDocument429 =
  addResponseDocument @"429"

addResponseBody431 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response431 a : tags)
addResponseBody431 =
  addResponseBody @"431"

addResponseDocument431 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response431 Document : tags)
addResponseDocument431 =
  addResponseDocument @"431"

addResponseBody500 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response500 a : tags)
addResponseBody500 =
  addResponseBody @"500"

addResponseDocument500 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response500 Document : tags)
addResponseDocument500 =
  addResponseDocument @"500"

addResponseBody501 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response501 a : tags)
addResponseBody501 =
  addResponseBody @"501"

addResponseDocument501 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response501 Document : tags)
addResponseDocument501 =
  addResponseDocument @"501"

addResponseBody502 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response502 a : tags)
addResponseBody502 =
  addResponseBody @"502"

addResponseDocument502 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response502 Document : tags)
addResponseDocument502 =
  addResponseDocument @"502"

addResponseBody503 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response503 a : tags)
addResponseBody503 =
  addResponseBody @"503"

addResponseDocument503 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response503 Document : tags)
addResponseDocument503 =
  addResponseDocument @"503"

addResponseBody504 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response504 a : tags)
addResponseBody504 =
  addResponseBody @"504"

addResponseDocument504 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response504 Document : tags)
addResponseDocument504 =
  addResponseDocument @"504"

addResponseBody505 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response505 a : tags)
addResponseBody505 =
  addResponseBody @"505"

addResponseDocument505 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response505 Document : tags)
addResponseDocument505 =
  addResponseDocument @"505"

addResponseBody511 ::
  ContentType ->
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response511 a : tags)
addResponseBody511 =
  addResponseBody @"511"

addResponseDocument511 ::
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response511 Document : tags)
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
