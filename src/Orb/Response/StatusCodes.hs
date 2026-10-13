{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Orb.Response.StatusCodes
  ( StatusCode (..)
  , mkStatusCode
  , status100
  , status101
  , status200
  , status201
  , status202
  , status203
  , status204
  , status205
  , status206
  , status300
  , status301
  , status302
  , status303
  , status304
  , status305
  , status307
  , status308
  , status400
  , status401
  , status402
  , status403
  , status404
  , status405
  , status406
  , status407
  , status408
  , status409
  , status410
  , status411
  , status412
  , status413
  , status414
  , status415
  , status416
  , status417
  , status418
  , status422
  , status428
  , status429
  , status431
  , status500
  , status501
  , status502
  , status503
  , status504
  , status505
  , status511
  , Response
  , ReturnType
  , addResponseSchema
  , addResponseBody
  , addResponseDocument
  , addResponseNoContent
  , returnResponse
  , returnResponseWithHeaders
  , mkResponse
  , mkResponseWithHeaders
  , addResponseBodyWithResponseContent
  )
where

import Data.ByteString.Builder (lazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Fleece.Aeson qualified as FA
import Fleece.Core qualified as FC
import GHC.TypeLits (KnownNat, KnownSymbol, SSymbol, Symbol, symbolSing)
import Network.HTTP.Types qualified as HTTP
import Shrubbery (type (@=))
import Shrubbery qualified as S

import Orb.Response.ContentType (ContentType, applicationJson)
import Orb.Response.Document (Document (..))
import Orb.Response.HasResponse (HasResponseCodeWithType)
import Orb.Response.Response (ResponseBodiesBuilder (..), ResponseBody (..), ResponseContent (..), ResponseData (..))
import Orb.Response.Schemas (NoContent (..))

data StatusCode (code :: Symbol) = StatusCode
  { statusCodeSymbol :: SSymbol code
  , statusCodeHTTPStatus :: HTTP.Status
  }

mkStatusCode :: KnownSymbol code => HTTP.Status -> StatusCode code
mkStatusCode =
  StatusCode symbolSing

type Response code a = code @= (a, HTTP.ResponseHeaders)

addResponseBody ::
  forall code tags a.
  StatusCode code ->
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response code a : tags)
addResponseBody statusCode contentType encoder =
  addResponseBodyWithResponseContent
    statusCode
    (Just contentType)
    (ResponseContentBuilder . lazyByteString . encoder)

{- | Adds a typed response for a given status code to the
    'ResponseBodiesBuilder'.

This function associates some type @a@ to the response code given by the
'StatusCode'. The response body will be the encoding of the type as encoded
by the provided encoding function, and the @Content-Type@ will be set with the
provided 'ContentType'.

@since 0.1.0
-}
addResponseBodyWithResponseContent ::
  forall code tags a.
  StatusCode code ->
  -- | The MIME type, as a 'BS.ByteString'.
  Maybe ContentType ->
  (a -> ResponseContent) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response code a : tags)
addResponseBodyWithResponseContent statusCode mbContentType mkResponseContent builder =
  let
    status =
      statusCodeHTTPStatus statusCode

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (value, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataContent = mkResponseContent value
        , responseDataContentType = mbContentType
        , responseDataExtraHeaders = headers
        }
  in
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @code runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insertWith (<>) status [NoSchemaResponseBody mbContentType] (responseStatusMapBuilder builder)
      }

{- | Adds a Fleece response for a given status code to the
    'ResponseBodiesBuilder'.

This function associates some type @a@ to the response code given by the
'StatusCode'. The response body will be the encoding of the type as encoded
by Fleece, and the @Content-Type@ will be set to /application\/json/.

@since 0.1.0
-}
addResponseSchema ::
  forall code tags a.
  StatusCode code ->
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response code a : tags)
addResponseSchema statusCode schema builder =
  let
    status =
      statusCodeHTTPStatus statusCode

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (value, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataContent = ResponseContentBuilder . lazyByteString $ FA.encode schema value
        , responseDataContentType = Just applicationJson
        , responseDataExtraHeaders = headers
        }
  in
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @code runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insertWith (<>) status [SchemaResponseBody schema] (responseStatusMapBuilder builder)
      }

{- | Adds a document response for a given status code to the
'ResponseBodiesBuilder'.

This function associates a 'Document' to the response code given by the
'StatusCode'. The response body will be the raw content from the provided
'Document', the @Content-Type@ will be set to the document's MIME type, and the
@Content-Disposition@ header is automatically generated using the document’s
file name.

@since 0.1.0
-}
addResponseDocument ::
  forall code tags.
  StatusCode code ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response code Document : tags)
addResponseDocument statusCode builder =
  let
    status =
      statusCodeHTTPStatus statusCode

    encodeDocument (document, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataContent = ResponseContentBuilder . lazyByteString $ documentContent document
        , responseDataContentType = Just $ documentType document
        , responseDataExtraHeaders =
            ( CI.mk $ BS8.pack "Content-Disposition"
            , BS8.pack "attachment;filename=" <> documentFileName document
            )
              : headers
        }
  in
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @code encodeDocument (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insertWith (<>) status [NoSchemaResponseBody Nothing] (responseStatusMapBuilder builder)
      }

{- | Associates a /no-content/ type response with a given status code in the
'ResponseBodiesBuilder'.

When a no-content response is constructed this way, the response body will be
empty, and the @Content-Type@ header will not be set.

@since 0.1.0
-}
addResponseNoContent ::
  forall code tags.
  StatusCode code ->
  ResponseBodiesBuilder tags ->
  ResponseBodiesBuilder (Response code NoContent : tags)
addResponseNoContent statusCode builder =
  let
    status =
      statusCodeHTTPStatus statusCode

    runEncoder :: (a, HTTP.ResponseHeaders) -> ResponseData
    runEncoder (_, headers) =
      ResponseData
        { responseDataStatus = status
        , responseDataContent = ResponseContentBuilder mempty
        , responseDataContentType = Nothing
        , responseDataExtraHeaders = headers
        }
  in
    ResponseBodiesBuilder
      { encodeResponseBranchesBuilder =
          S.taggedBranch @code runEncoder (encodeResponseBranchesBuilder builder)
      , responseStatusMapBuilder =
          Map.insertWith (<>) status [EmptyResponseBody] (responseStatusMapBuilder builder)
      }

status100 :: StatusCode "100"
status100 = mkStatusCode HTTP.status100

status101 :: StatusCode "101"
status101 = mkStatusCode HTTP.status101

status200 :: StatusCode "200"
status200 = mkStatusCode HTTP.status200

status201 :: StatusCode "201"
status201 = mkStatusCode HTTP.status201

status202 :: StatusCode "202"
status202 = mkStatusCode HTTP.status202

status203 :: StatusCode "203"
status203 = mkStatusCode HTTP.status203

status204 :: StatusCode "204"
status204 = mkStatusCode HTTP.status204

status205 :: StatusCode "205"
status205 = mkStatusCode HTTP.status205

status206 :: StatusCode "206"
status206 = mkStatusCode HTTP.status206

status300 :: StatusCode "300"
status300 = mkStatusCode HTTP.status300

status301 :: StatusCode "301"
status301 = mkStatusCode HTTP.status301

status302 :: StatusCode "302"
status302 = mkStatusCode HTTP.status302

status303 :: StatusCode "303"
status303 = mkStatusCode HTTP.status303

status304 :: StatusCode "304"
status304 = mkStatusCode HTTP.status304

status305 :: StatusCode "305"
status305 = mkStatusCode HTTP.status305

status307 :: StatusCode "307"
status307 = mkStatusCode HTTP.status307

status308 :: StatusCode "308"
status308 = mkStatusCode HTTP.status308

status400 :: StatusCode "400"
status400 = mkStatusCode HTTP.status400

status401 :: StatusCode "401"
status401 = mkStatusCode HTTP.status401

status402 :: StatusCode "402"
status402 = mkStatusCode HTTP.status402

status403 :: StatusCode "403"
status403 = mkStatusCode HTTP.status403

status404 :: StatusCode "404"
status404 = mkStatusCode HTTP.status404

status405 :: StatusCode "405"
status405 = mkStatusCode HTTP.status405

status406 :: StatusCode "406"
status406 = mkStatusCode HTTP.status406

status407 :: StatusCode "407"
status407 = mkStatusCode HTTP.status407

status408 :: StatusCode "408"
status408 = mkStatusCode HTTP.status408

status409 :: StatusCode "409"
status409 = mkStatusCode HTTP.status409

status410 :: StatusCode "410"
status410 = mkStatusCode HTTP.status410

status411 :: StatusCode "411"
status411 = mkStatusCode HTTP.status411

status412 :: StatusCode "412"
status412 = mkStatusCode HTTP.status412

status413 :: StatusCode "413"
status413 = mkStatusCode HTTP.status413

status414 :: StatusCode "414"
status414 = mkStatusCode HTTP.status414

status415 :: StatusCode "415"
status415 = mkStatusCode HTTP.status415

status416 :: StatusCode "416"
status416 = mkStatusCode HTTP.status416

status417 :: StatusCode "417"
status417 = mkStatusCode HTTP.status417

status418 :: StatusCode "418"
status418 = mkStatusCode HTTP.status418

status422 :: StatusCode "422"
status422 = mkStatusCode HTTP.status422

status428 :: StatusCode "428"
status428 = mkStatusCode HTTP.status428

status429 :: StatusCode "429"
status429 = mkStatusCode HTTP.status429

status431 :: StatusCode "431"
status431 = mkStatusCode HTTP.status431

status500 :: StatusCode "500"
status500 = mkStatusCode HTTP.status500

status501 :: StatusCode "501"
status501 = mkStatusCode HTTP.status501

status502 :: StatusCode "502"
status502 = mkStatusCode HTTP.status502

status503 :: StatusCode "503"
status503 = mkStatusCode HTTP.status503

status504 :: StatusCode "504"
status504 = mkStatusCode HTTP.status504

status505 :: StatusCode "505"
status505 = mkStatusCode HTTP.status505

status511 :: StatusCode "511"
status511 = mkStatusCode HTTP.status511

{- | A type synonym that encodes how a handler function can produce a typed
response for a given status code in a monadic context.

The key idea is that @returnType@ represents the actual response body type
(e.g., a JSON-serializable record or a custom document), while the underlying
constraint ensures it corresponds correctly to a status @code@ within the
'S.TaggedUnion' of responses (parameterized by @responseCodes@). It also
requires @m@ to have an instance of an 'Applicative'.

This type alias is primarily used to define response functions such as
'returnResponse', which take the @code@ from a 'StatusCode' argument. That
gives you a convenient way to ensure that any function returning them lines up
exactly with the response codes in the 'ResponseBodiesBuilder'.

@since 0.1.0
-}
type ReturnType returnType index m responseCodes code =
  ( S.LookupTag code responseCodes ~ '(index, (returnType, HTTP.ResponseHeaders))
  , KnownNat index
  , Applicative m
  ) =>
  returnType ->
  m (S.TaggedUnion responseCodes)

returnResponse ::
  forall code returnType index m responseCodes.
  StatusCode code ->
  ReturnType returnType index m responseCodes code
returnResponse statusCode =
  returnResponseWithHeaders statusCode []

returnResponseWithHeaders ::
  forall code returnType index m responseCodes.
  StatusCode code ->
  HTTP.ResponseHeaders ->
  ReturnType returnType index m responseCodes code
returnResponseWithHeaders statusCode headers =
  pure . mkResponseWithHeaders statusCode headers

mkResponse ::
  forall code responseType tags.
  HasResponseCodeWithType tags code responseType =>
  StatusCode code ->
  responseType ->
  S.TaggedUnion tags
mkResponse statusCode =
  mkResponseWithHeaders statusCode []

mkResponseWithHeaders ::
  forall code responseType tags.
  HasResponseCodeWithType tags code responseType =>
  StatusCode code ->
  HTTP.ResponseHeaders ->
  responseType ->
  S.TaggedUnion tags
mkResponseWithHeaders _statusCode headers =
  S.unifyTaggedUnion @code . (,headers)
