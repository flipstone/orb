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
  , addResponse100
  , addResponse101
  , addResponseDocument200
  , addResponseBody200
  , addResponseSchema200
  , addResponseDocument201
  , addResponseBody201
  , addResponseSchema201
  , addResponseDocument202
  , addResponseBody202
  , addResponseSchema202
  , addResponseDocument203
  , addResponseBody203
  , addResponseSchema203
  , addResponse204
  , addResponse205
  , addResponseDocument206
  , addResponseBody206
  , addResponseSchema206
  , addResponseDocument300
  , addResponseBody300
  , addResponseSchema300
  , addResponseDocument301
  , addResponseBody301
  , addResponseSchema301
  , addResponseDocument302
  , addResponseBody302
  , addResponseSchema302
  , addResponseDocument303
  , addResponseBody303
  , addResponseSchema303
  , addResponse304
  , addResponseDocument305
  , addResponseBody305
  , addResponseSchema305
  , addResponseDocument307
  , addResponseBody307
  , addResponseSchema307
  , addResponseDocument308
  , addResponseBody308
  , addResponseSchema308
  , addResponseDocument400
  , addResponseBody400
  , addResponseSchema400
  , addResponseDocument401
  , addResponseBody401
  , addResponseSchema401
  , addResponseDocument402
  , addResponseBody402
  , addResponseSchema402
  , addResponseDocument403
  , addResponseBody403
  , addResponseSchema403
  , addResponseDocument404
  , addResponseBody404
  , addResponseSchema404
  , addResponseDocument405
  , addResponseBody405
  , addResponseSchema405
  , addResponseDocument406
  , addResponseBody406
  , addResponseSchema406
  , addResponseDocument407
  , addResponseBody407
  , addResponseSchema407
  , addResponseDocument408
  , addResponseBody408
  , addResponseSchema408
  , addResponseDocument409
  , addResponseBody409
  , addResponseSchema409
  , addResponseDocument410
  , addResponseBody410
  , addResponseSchema410
  , addResponseDocument411
  , addResponseBody411
  , addResponseSchema411
  , addResponseDocument412
  , addResponseBody412
  , addResponseSchema412
  , addResponseDocument413
  , addResponseBody413
  , addResponseSchema413
  , addResponseDocument414
  , addResponseBody414
  , addResponseSchema414
  , addResponseDocument415
  , addResponseBody415
  , addResponseSchema415
  , addResponseDocument416
  , addResponseBody416
  , addResponseSchema416
  , addResponseDocument417
  , addResponseBody417
  , addResponseSchema417
  , addResponseDocument418
  , addResponseBody418
  , addResponseSchema418
  , addResponseDocument422
  , addResponseBody422
  , addResponseSchema422
  , addResponseDocument428
  , addResponseBody428
  , addResponseSchema428
  , addResponseDocument429
  , addResponseBody429
  , addResponseSchema429
  , addResponseDocument431
  , addResponseBody431
  , addResponseSchema431
  , addResponseDocument500
  , addResponseBody500
  , addResponseSchema500
  , addResponseDocument501
  , addResponseBody501
  , addResponseSchema501
  , addResponseDocument502
  , addResponseBody502
  , addResponseSchema502
  , addResponseDocument503
  , addResponseBody503
  , addResponseSchema503
  , addResponseDocument504
  , addResponseBody504
  , addResponseSchema504
  , addResponseDocument505
  , addResponseBody505
  , addResponseSchema505
  , addResponseDocument511
  , addResponseBody511
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

{- | A type synonym representing an HTTP 100 (Continue) response with no
content.

/Example usage/:

@
type ResponseCodes =
  '[ Response100
   ]
@

@since 0.1.0
-}
type Response100 = Response "100" NoContent

{- | A type synonym representing an HTTP 101 (Switching Protocols) response
with no content.

/Example usage/:

@
type ResponseCodes =
  '[ Response101
   ]
@

@since 0.1.0
-}
type Response101 = Response "101" NoContent

{- | A type synonym representing an HTTP 200 (OK) response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response200 MyType
   ]
@

@since 0.1.0
-}
type Response200 a = Response "200" a

{- | A type synonym representing an HTTP 201 (Created) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response201 MyType
   ]
@

@since 0.1.0
-}
type Response201 a = Response "201" a

{- | A type synonym representing an HTTP 202 (Accepted) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response202 MyType
   ]
@

@since 0.1.0
-}
type Response202 a = Response "202" a

{- | A type synonym representing an HTTP 203 (Non-Authoritative Information)
response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response203 MyType
   ]
@

@since 0.1.0
-}
type Response203 a = Response "203" a

{- | A type synonym representing an HTTP 204 (No Content) response with no
content.

/Example usage/:

@
type ResponseCodes =
  '[ Response204
   ]
@

@since 0.1.0
-}
type Response204 = Response "204" NoContent

{- | A type synonym representing an HTTP 205 (Reset Content) response with no
content.

/Example usage/:

@
type ResponseCodes =
  '[ Response205
   ]
@

@since 0.1.0
-}
type Response205 = Response "205" NoContent

{- | A type synonym representing an HTTP 206 (Partial Content) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response206 MyType
   ]
@

@since 0.1.0
-}
type Response206 a = Response "206" a

{- | A type synonym representing an HTTP 300 (Multiple Choices) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response300 MyType
   ]
@

@since 0.1.0
-}
type Response300 a = Response "300" a

{- | A type synonym representing an HTTP 301 (Moved Permanently) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response301 MyType
   ]
@

@since 0.1.0
-}
type Response301 a = Response "301" a

{- | A type synonym representing an HTTP 302 (Found) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response302 MyType
   ]
@

@since 0.1.0
-}
type Response302 a = Response "302" a

{- | A type synonym representing an HTTP 303 (See Other) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response303 MyType
   ]
@

@since 0.1.0
-}
type Response303 a = Response "303" a

{- | A type synonym representing an HTTP 304 (Not Modified) response with no
content.

/Example usage/:

@
type ResponseCodes =
  '[ Response304
   ]
@

@since 0.1.0
-}
type Response304 = Response "304" NoContent

{- | A type synonym representing an HTTP 305 (Use Proxy) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response305 MyType
   ]
@

@since 0.1.0
-}
type Response305 a = Response "305" a

{- | A type synonym representing an HTTP 307 (Temporary Redirect) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response307 MyType
   ]
@

@since 0.1.0
-}
type Response307 a = Response "307" a

{- | A type synonym representing an HTTP 308 (Permanent Redirect) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response308 MyType
   ]
@

@since 0.1.0
-}
type Response308 a = Response "308" a

{- | A type synonym representing an HTTP 400 (Bad Request) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response400 MyType
   ]
@

@since 0.1.0
-}
type Response400 a = Response "400" a

{- | A type synonym representing an HTTP 401 (Unauthorized) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response401 MyType
   ]
@

@since 0.1.0
-}
type Response401 a = Response "401" a

{- | A type synonym representing an HTTP 402 (Payment Required) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response402 MyType
   ]
@

@since 0.1.0
-}
type Response402 a = Response "402" a

{- | A type synonym representing an HTTP 403 (Forbidden) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response403 MyType
   ]
@

@since 0.1.0
-}
type Response403 a = Response "403" a

{- | A type synonym representing an HTTP 404 (Not Found) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response404 MyType
   ]
@

@since 0.1.0
-}
type Response404 a = Response "404" a

{- | A type synonym representing an HTTP 405 (Method Not Allowed) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response405 MyType
   ]
@

@since 0.1.0
-}
type Response405 a = Response "405" a

{- | A type synonym representing an HTTP 406 (Not Acceptable) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response406 MyType
   ]
@

@since 0.1.0
-}
type Response406 a = Response "406" a

{- | A type synonym representing an HTTP 407 (Proxy Authentication Required)
response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response407 MyType
   ]
@

@since 0.1.0
-}
type Response407 a = Response "407" a

{- | A type synonym representing an HTTP 408 (Request Timeout) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response408 MyType
   ]
@

@since 0.1.0
-}
type Response408 a = Response "408" a

{- | A type synonym representing an HTTP 409 (Conflict) response with a typed
body.

/Example usage/:

@
type ResponseCodes =
  '[ Response409 MyType
   ]
@

@since 0.1.0
-}
type Response409 a = Response "409" a

{- | A type synonym representing an HTTP 410 (Gone) response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response410 MyType
   ]
@

@since 0.1.0
-}
type Response410 a = Response "410" a

{- | A type synonym representing an HTTP 411 (Length Required) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response411 MyType
   ]
@

@since 0.1.0
-}
type Response411 a = Response "411" a

{- | A type synonym representing an HTTP 412 (Precondition Failed) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response412 MyType
   ]
@

@since 0.1.0
-}
type Response412 a = Response "412" a

{- | A type synonym representing an HTTP 413 (Payload Too Large) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response413 MyType
   ]
@

@since 0.1.0
-}
type Response413 a = Response "413" a

{- | A type synonym representing an HTTP 414 (URI Too Long) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response414 MyType
   ]
@

@since 0.1.0
-}
type Response414 a = Response "414" a

{- | A type synonym representing an HTTP 415 (Unsupported Media Type) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response415 MyType
   ]
@

@since 0.1.0
-}
type Response415 a = Response "415" a

{- | A type synonym representing an HTTP 416 (Range Not Satisfiable) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response416 MyType
   ]
@

@since 0.1.0
-}
type Response416 a = Response "416" a

{- | A type synonym representing an HTTP 417 (Expectation Failed) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response417 MyType
   ]
@

@since 0.1.0
-}
type Response417 a = Response "417" a

{- | A type synonym representing an HTTP 418 (I'm a Teapot) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response418 MyType
   ]
@

@since 0.1.0
-}
type Response418 a = Response "418" a

{- | A type synonym representing an HTTP 422 (Unprocessable Entity) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response422 MyType
   ]
@

@since 0.1.0
-}
type Response422 a = Response "422" a

{- | A type synonym representing an HTTP 428 (Precondition Required) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response428 MyType
   ]
@

@since 0.1.0
-}
type Response428 a = Response "428" a

{- | A type synonym representing an HTTP 429 (Too Many Requests) response with
a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response429 MyType
   ]
@

@since 0.1.0
-}
type Response429 a = Response "429" a

{- | A type synonym representing an HTTP 431 (Request Header Fields Too Large)
response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response431 MyType
   ]
@

@since 0.1.0
-}
type Response431 a = Response "431" a

{- | A type synonym representing an HTTP 500 (Internal Server Error) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response500 MyType
   ]
@

@since 0.1.0
-}
type Response500 a = Response "500" a

{- | A type synonym representing an HTTP 501 (Not Implemented) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response501 MyType
   ]
@

@since 0.1.0
-}
type Response501 a = Response "501" a

{- | A type synonym representing an HTTP 502 (Bad Gateway) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response502 MyType
   ]
@

@since 0.1.0
-}
type Response502 a = Response "502" a

{- | A type synonym representing an HTTP 503 (Service Unavailable) response
with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response503 MyType
   ]
@

@since 0.1.0
-}
type Response503 a = Response "503" a

{- | A type synonym representing an HTTP 504 (Gateway Timeout) response with a
typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response504 MyType
   ]
@

@since 0.1.0
-}
type Response504 a = Response "504" a

{- | A type synonym representing an HTTP 505 (HTTP Version Not Supported)
response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response505 MyType
   ]
@

@since 0.1.0
-}
type Response505 a = Response "505" a

{- | A type synonym representing an HTTP 511 (Network Authentication Required)
response with a typed body.

/Example usage/:

@
type ResponseCodes =
  '[ Response511 MyType
   ]
@

@since 0.1.0
-}
type Response511 a = Response "511" a

{- | Appends an HTTP 100 (Continue) no-content response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponse100
  $ noResponseBodies
@

@since 0.1.0
-}
addResponse100 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response100 : responseCodes)
addResponse100 =
  addResponseNoContent status100

{- | Appends an HTTP 101 (Switching Protocols) no-content response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponse101
  $ noResponseBodies
@

@since 0.1.0
-}
addResponse101 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response101 : responseCodes)
addResponse101 =
  addResponseNoContent status101

{- | Appends an HTTP 200 (OK) typed response to the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody200 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody200 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response200 a : responseCodes)
addResponseBody200 =
  addResponseBody status200

{- | Appends an HTTP 200 (OK) Fleece response to the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema200 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema200 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response200 a : responseCodes)
addResponseSchema200 =
  addResponseSchema status200

{- | Appends an HTTP 200 (OK) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument200
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument200 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response200 Document : responseCodes)
addResponseDocument200 =
  addResponseDocument status200

{- | Appends an HTTP 201 (Created) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody201 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody201 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response201 a : responseCodes)
addResponseBody201 =
  addResponseBody status201

{- | Appends an HTTP 201 (Created) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema201 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema201 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response201 a : responseCodes)
addResponseSchema201 =
  addResponseSchema status201

{- | Appends an HTTP 201 (Created) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument201
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument201 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response201 Document : responseCodes)
addResponseDocument201 =
  addResponseDocument status201

{- | Appends an HTTP 202 (Accepted) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody202 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody202 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response202 a : responseCodes)
addResponseBody202 =
  addResponseBody status202

{- | Appends an HTTP 202 (Accepted) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema202 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema202 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response202 a : responseCodes)
addResponseSchema202 =
  addResponseSchema status202

{- | Appends an HTTP 202 (Accepted) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument202
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument202 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response202 Document : responseCodes)
addResponseDocument202 =
  addResponseDocument status202

{- | Appends an HTTP 203 (Non-Authoritative Information) typed response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody203 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody203 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response203 a : responseCodes)
addResponseBody203 =
  addResponseBody status203

{- | Appends an HTTP 203 (Non-Authoritative Information) Fleece response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema203 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema203 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response203 a : responseCodes)
addResponseSchema203 =
  addResponseSchema status203

{- | Appends an HTTP 203 (Non-Authoritative Information) document response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument203
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument203 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response203 Document : responseCodes)
addResponseDocument203 =
  addResponseDocument status203

{- | Appends an HTTP 204 (No Content) no-content response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponse204
  $ noResponseBodies
@

@since 0.1.0
-}
addResponse204 ::
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response204 : responseCodes)
addResponse204 =
  addResponseNoContent status204

{- | Appends an HTTP 205 (Reset Content) no-content response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponse205
  $ noResponseBodies
@

@since 0.1.0
-}
addResponse205 ::
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response205 : responseCodes)
addResponse205 =
  addResponseNoContent status205

{- | Appends an HTTP 206 (Partial Content) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody206 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody206 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response206 a : responseCodes)
addResponseBody206 =
  addResponseBody status206

{- | Appends an HTTP 206 (Partial Content) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema206 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema206 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response206 a : responseCodes)
addResponseSchema206 =
  addResponseSchema status206

{- | Appends an HTTP 206 (Partial Content) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument206
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument206 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response206 Document : responseCodes)
addResponseDocument206 =
  addResponseDocument status206

{- | Appends an HTTP 300 (Multiple Choices) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody300 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody300 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response300 a : responseCodes)
addResponseBody300 =
  addResponseBody status300

{- | Appends an HTTP 300 (Multiple Choices) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema300 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema300 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response300 a : responseCodes)
addResponseSchema300 =
  addResponseSchema status300

{- | Appends an HTTP 300 (Multiple Choices) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument300
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument300 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response300 Document : responseCodes)
addResponseDocument300 =
  addResponseDocument status300

{- | Appends an HTTP 301 (Moved Permanently) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody301 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody301 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response301 a : responseCodes)
addResponseBody301 =
  addResponseBody status301

{- | Appends an HTTP 301 (Moved Permanently) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema301 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema301 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response301 a : responseCodes)
addResponseSchema301 =
  addResponseSchema status301

{- | Appends an HTTP 301 (Moved Permanently) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument301
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument301 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response301 Document : responseCodes)
addResponseDocument301 =
  addResponseDocument status301

{- | Appends an HTTP 302 (Found) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody302 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody302 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response302 a : responseCodes)
addResponseBody302 =
  addResponseBody status302

{- | Appends an HTTP 302 (Found) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema302 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema302 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response302 a : responseCodes)
addResponseSchema302 =
  addResponseSchema status302

{- | Appends an HTTP 302 (Found) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument302
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument302 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response302 Document : responseCodes)
addResponseDocument302 =
  addResponseDocument status302

{- | Appends an HTTP 303 (See Other) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody303 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody303 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response303 a : responseCodes)
addResponseBody303 =
  addResponseBody status303

{- | Appends an HTTP 303 (See Other) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema303 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema303 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response303 a : responseCodes)
addResponseSchema303 =
  addResponseSchema status303

{- | Appends an HTTP 303 (See Other) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument303
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument303 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response303 Document : responseCodes)
addResponseDocument303 =
  addResponseDocument status303

{- | Appends an HTTP 304 (Not Modified) no-content response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponse304
  $ noResponseBodies
@

@since 0.1.0
-}
addResponse304 ::
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response304 : responseCodes)
addResponse304 =
  addResponseNoContent status304

{- | Appends an HTTP 305 (Use Proxy) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody305 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody305 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response305 a : responseCodes)
addResponseBody305 =
  addResponseBody status305

{- | Appends an HTTP 305 (Use Proxy) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema305 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema305 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response305 a : responseCodes)
addResponseSchema305 =
  addResponseSchema status305

{- | Appends an HTTP 305 (Use Proxy) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument305
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument305 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response305 Document : responseCodes)
addResponseDocument305 =
  addResponseDocument status305

{- | Appends an HTTP 307 (Temporary Redirect) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody307 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody307 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response307 a : responseCodes)
addResponseBody307 =
  addResponseBody status307

{- | Appends an HTTP 307 (Temporary Redirect) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema307 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema307 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response307 a : responseCodes)
addResponseSchema307 =
  addResponseSchema status307

{- | Appends an HTTP 307 (Temporary Redirect) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument307
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument307 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response307 Document : responseCodes)
addResponseDocument307 =
  addResponseDocument status307

{- | Appends an HTTP 308 (Permanent Redirect) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody308 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody308 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response308 a : responseCodes)
addResponseBody308 =
  addResponseBody status308

{- | Appends an HTTP 308 (Permanent Redirect) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema308 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema308 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response308 a : responseCodes)
addResponseSchema308 =
  addResponseSchema status308

{- | Appends an HTTP 308 (Permanent Redirect) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument308
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument308 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response308 Document : responseCodes)
addResponseDocument308 =
  addResponseDocument status308

{- | Appends an HTTP 400 (Bad Request) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody400 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody400 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response400 a : responseCodes)
addResponseBody400 =
  addResponseBody status400

{- | Appends an HTTP 400 (Bad Request) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema400 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema400 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response400 a : responseCodes)
addResponseSchema400 =
  addResponseSchema status400

{- | Appends an HTTP 400 (Bad Request) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument400
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument400 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response400 Document : responseCodes)
addResponseDocument400 =
  addResponseDocument status400

{- | Appends an HTTP 401 (Unauthorized) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody401 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody401 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response401 a : responseCodes)
addResponseBody401 =
  addResponseBody status401

{- | Appends an HTTP 401 (Unauthorized) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema401 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema401 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response401 a : responseCodes)
addResponseSchema401 =
  addResponseSchema status401

{- | Appends an HTTP 401 (Unauthorized) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument401
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument401 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response401 Document : responseCodes)
addResponseDocument401 =
  addResponseDocument status401

{- | Appends an HTTP 402 (Payment Required) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody402 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody402 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response402 a : responseCodes)
addResponseBody402 =
  addResponseBody status402

{- | Appends an HTTP 402 (Payment Required) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema402 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema402 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response402 a : responseCodes)
addResponseSchema402 =
  addResponseSchema status402

{- | Appends an HTTP 402 (Payment Required) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument402
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument402 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response402 Document : responseCodes)
addResponseDocument402 =
  addResponseDocument status402

{- | Appends an HTTP 403 (Forbidden) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody403 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody403 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response403 a : responseCodes)
addResponseBody403 =
  addResponseBody status403

{- | Appends an HTTP 403 (Forbidden) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema403 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema403 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response403 a : responseCodes)
addResponseSchema403 =
  addResponseSchema status403

{- | Appends an HTTP 403 (Forbidden) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument403
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument403 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response403 Document : responseCodes)
addResponseDocument403 =
  addResponseDocument status403

{- | Appends an HTTP 404 (Not Found) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody404 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody404 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response404 a : responseCodes)
addResponseBody404 =
  addResponseBody status404

{- | Appends an HTTP 404 (Not Found) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema404 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema404 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response404 a : responseCodes)
addResponseSchema404 =
  addResponseSchema status404

{- | Appends an HTTP 404 (Not Found) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument404
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument404 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response404 Document : responseCodes)
addResponseDocument404 =
  addResponseDocument status404

{- | Appends an HTTP 405 (Method Not Allowed) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody405 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody405 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response405 a : responseCodes)
addResponseBody405 =
  addResponseBody status405

{- | Appends an HTTP 405 (Method Not Allowed) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema405 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema405 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response405 a : responseCodes)
addResponseSchema405 =
  addResponseSchema status405

{- | Appends an HTTP 405 (Method Not Allowed) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument405
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument405 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response405 Document : responseCodes)
addResponseDocument405 =
  addResponseDocument status405

{- | Appends an HTTP 406 (Not Acceptable) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody406 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody406 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response406 a : responseCodes)
addResponseBody406 =
  addResponseBody status406

{- | Appends an HTTP 406 (Not Acceptable) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema406 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema406 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response406 a : responseCodes)
addResponseSchema406 =
  addResponseSchema status406

{- | Appends an HTTP 406 (Not Acceptable) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument406
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument406 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response406 Document : responseCodes)
addResponseDocument406 =
  addResponseDocument status406

{- | Appends an HTTP 407 (Proxy Authentication Required) typed response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody407 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody407 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response407 a : responseCodes)
addResponseBody407 =
  addResponseBody status407

{- | Appends an HTTP 407 (Proxy Authentication Required) Fleece response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema407 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema407 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response407 a : responseCodes)
addResponseSchema407 =
  addResponseSchema status407

{- | Appends an HTTP 407 (Proxy Authentication Required) document response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument407
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument407 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response407 Document : responseCodes)
addResponseDocument407 =
  addResponseDocument status407

{- | Appends an HTTP 408 (Request Timeout) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody408 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody408 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response408 a : responseCodes)
addResponseBody408 =
  addResponseBody status408

{- | Appends an HTTP 408 (Request Timeout) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema408 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema408 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response408 a : responseCodes)
addResponseSchema408 =
  addResponseSchema status408

{- | Appends an HTTP 408 (Request Timeout) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument408
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument408 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response408 Document : responseCodes)
addResponseDocument408 =
  addResponseDocument status408

{- | Appends an HTTP 409 (Conflict) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody409 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody409 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response409 a : responseCodes)
addResponseBody409 =
  addResponseBody status409

{- | Appends an HTTP 409 (Conflict) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema409 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema409 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response409 a : responseCodes)
addResponseSchema409 =
  addResponseSchema status409

{- | Appends an HTTP 409 (Conflict) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument409
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument409 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response409 Document : responseCodes)
addResponseDocument409 =
  addResponseDocument status409

{- | Appends an HTTP 410 (Gone) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody410 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody410 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response410 a : responseCodes)
addResponseBody410 =
  addResponseBody status410

{- | Appends an HTTP 410 (Gone) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema410 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema410 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response410 a : responseCodes)
addResponseSchema410 =
  addResponseSchema status410

{- | Appends an HTTP 410 (Gone) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument410
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument410 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response410 Document : responseCodes)
addResponseDocument410 =
  addResponseDocument status410

{- | Appends an HTTP 411 (Length Required) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody411 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody411 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response411 a : responseCodes)
addResponseBody411 =
  addResponseBody status411

{- | Appends an HTTP 411 (Length Required) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema411 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema411 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response411 a : responseCodes)
addResponseSchema411 =
  addResponseSchema status411

{- | Appends an HTTP 411 (Length Required) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument411
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument411 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response411 Document : responseCodes)
addResponseDocument411 =
  addResponseDocument status411

{- | Appends an HTTP 412 (Precondition Failed) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody412 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody412 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response412 a : responseCodes)
addResponseBody412 =
  addResponseBody status412

{- | Appends an HTTP 412 (Precondition Failed) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema412 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema412 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response412 a : responseCodes)
addResponseSchema412 =
  addResponseSchema status412

{- | Appends an HTTP 412 (Precondition Failed) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument412
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument412 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response412 Document : responseCodes)
addResponseDocument412 =
  addResponseDocument status412

{- | Appends an HTTP 413 (Payload Too Large) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody413 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody413 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response413 a : responseCodes)
addResponseBody413 =
  addResponseBody status413

{- | Appends an HTTP 413 (Payload Too Large) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema413 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema413 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response413 a : responseCodes)
addResponseSchema413 =
  addResponseSchema status413

{- | Appends an HTTP 413 (Payload Too Large) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument413
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument413 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response413 Document : responseCodes)
addResponseDocument413 =
  addResponseDocument status413

{- | Appends an HTTP 414 (URI Too Long) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody414 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody414 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response414 a : responseCodes)
addResponseBody414 =
  addResponseBody status414

{- | Appends an HTTP 414 (URI Too Long) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema414 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema414 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response414 a : responseCodes)
addResponseSchema414 =
  addResponseSchema status414

{- | Appends an HTTP 414 (URI Too Long) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument414
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument414 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response414 Document : responseCodes)
addResponseDocument414 =
  addResponseDocument status414

{- | Appends an HTTP 415 (Unsupported Media Type) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody415 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody415 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response415 a : responseCodes)
addResponseBody415 =
  addResponseBody status415

{- | Appends an HTTP 415 (Unsupported Media Type) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema415 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema415 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response415 a : responseCodes)
addResponseSchema415 =
  addResponseSchema status415

{- | Appends an HTTP 415 (Unsupported Media Type) document response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument415
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument415 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response415 Document : responseCodes)
addResponseDocument415 =
  addResponseDocument status415

{- | Appends an HTTP 416 (Range Not Satisfiable) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody416 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody416 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response416 a : responseCodes)
addResponseBody416 =
  addResponseBody status416

{- | Appends an HTTP 416 (Range Not Satisfiable) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema416 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema416 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response416 a : responseCodes)
addResponseSchema416 =
  addResponseSchema status416

{- | Appends an HTTP 416 (Range Not Satisfiable) document response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument416
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument416 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response416 Document : responseCodes)
addResponseDocument416 =
  addResponseDocument status416

{- | Appends an HTTP 417 (Expectation Failed) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody417 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody417 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response417 a : responseCodes)
addResponseBody417 =
  addResponseBody status417

{- | Appends an HTTP 417 (Expectation Failed) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema417 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema417 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response417 a : responseCodes)
addResponseSchema417 =
  addResponseSchema status417

{- | Appends an HTTP 417 (Expectation Failed) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument417
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument417 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response417 Document : responseCodes)
addResponseDocument417 =
  addResponseDocument status417

{- | Appends an HTTP 418 (I'm a Teapot) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody418 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody418 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response418 a : responseCodes)
addResponseBody418 =
  addResponseBody status418

{- | Appends an HTTP 418 (I'm a Teapot) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema418 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema418 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response418 a : responseCodes)
addResponseSchema418 =
  addResponseSchema status418

{- | Appends an HTTP 418 (I'm a Teapot) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument418
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument418 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response418 Document : responseCodes)
addResponseDocument418 =
  addResponseDocument status418

{- | Appends an HTTP 422 (Unprocessable Entity) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody422 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody422 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response422 a : responseCodes)
addResponseBody422 =
  addResponseBody status422

{- | Appends an HTTP 422 (Unprocessable Entity) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema422 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema422 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response422 a : responseCodes)
addResponseSchema422 =
  addResponseSchema status422

{- | Appends an HTTP 422 (Unprocessable Entity) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument422
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument422 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response422 Document : responseCodes)
addResponseDocument422 =
  addResponseDocument status422

{- | Appends an HTTP 428 (Precondition Required) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody428 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody428 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response428 a : responseCodes)
addResponseBody428 =
  addResponseBody status428

{- | Appends an HTTP 428 (Precondition Required) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema428 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema428 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response428 a : responseCodes)
addResponseSchema428 =
  addResponseSchema status428

{- | Appends an HTTP 428 (Precondition Required) document response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument428
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument428 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response428 Document : responseCodes)
addResponseDocument428 =
  addResponseDocument status428

{- | Appends an HTTP 429 (Too Many Requests) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody429 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody429 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response429 a : responseCodes)
addResponseBody429 =
  addResponseBody status429

{- | Appends an HTTP 429 (Too Many Requests) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema429 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema429 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response429 a : responseCodes)
addResponseSchema429 =
  addResponseSchema status429

{- | Appends an HTTP 429 (Too Many Requests) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument429
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument429 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response429 Document : responseCodes)
addResponseDocument429 =
  addResponseDocument status429

{- | Appends an HTTP 431 (Request Header Fields Too Large) typed response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody431 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody431 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response431 a : responseCodes)
addResponseBody431 =
  addResponseBody status431

{- | Appends an HTTP 431 (Request Header Fields Too Large) Fleece response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema431 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema431 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response431 a : responseCodes)
addResponseSchema431 =
  addResponseSchema status431

{- | Appends an HTTP 431 (Request Header Fields Too Large) document response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument431
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument431 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response431 Document : responseCodes)
addResponseDocument431 =
  addResponseDocument status431

{- | Appends an HTTP 500 (Internal Server Error) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody500 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody500 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response500 a : responseCodes)
addResponseBody500 =
  addResponseBody status500

{- | Appends an HTTP 500 (Internal Server Error) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema500 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema500 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response500 a : responseCodes)
addResponseSchema500 =
  addResponseSchema status500

{- | Appends an HTTP 500 (Internal Server Error) document response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument500
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument500 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response500 Document : responseCodes)
addResponseDocument500 =
  addResponseDocument status500

{- | Appends an HTTP 501 (Not Implemented) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody501 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody501 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response501 a : responseCodes)
addResponseBody501 =
  addResponseBody status501

{- | Appends an HTTP 501 (Not Implemented) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema501 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema501 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response501 a : responseCodes)
addResponseSchema501 =
  addResponseSchema status501

{- | Appends an HTTP 501 (Not Implemented) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument501
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument501 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response501 Document : responseCodes)
addResponseDocument501 =
  addResponseDocument status501

{- | Appends an HTTP 502 (Bad Gateway) typed response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseBody502 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody502 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response502 a : responseCodes)
addResponseBody502 =
  addResponseBody status502

{- | Appends an HTTP 502 (Bad Gateway) Fleece response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseSchema502 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema502 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response502 a : responseCodes)
addResponseSchema502 =
  addResponseSchema status502

{- | Appends an HTTP 502 (Bad Gateway) document response to the set of possible
responses.

/Example usage/:

@
responseBodies
  . addResponseDocument502
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument502 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response502 Document : responseCodes)
addResponseDocument502 =
  addResponseDocument status502

{- | Appends an HTTP 503 (Service Unavailable) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody503 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody503 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response503 a : responseCodes)
addResponseBody503 =
  addResponseBody status503

{- | Appends an HTTP 503 (Service Unavailable) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema503 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema503 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response503 a : responseCodes)
addResponseSchema503 =
  addResponseSchema status503

{- | Appends an HTTP 503 (Service Unavailable) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument503
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument503 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response503 Document : responseCodes)
addResponseDocument503 =
  addResponseDocument status503

{- | Appends an HTTP 504 (Gateway Timeout) typed response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody504 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody504 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response504 a : responseCodes)
addResponseBody504 =
  addResponseBody status504

{- | Appends an HTTP 504 (Gateway Timeout) Fleece response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema504 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema504 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response504 a : responseCodes)
addResponseSchema504 =
  addResponseSchema status504

{- | Appends an HTTP 504 (Gateway Timeout) document response to the set of
possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument504
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument504 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response504 Document : responseCodes)
addResponseDocument504 =
  addResponseDocument status504

{- | Appends an HTTP 505 (HTTP Version Not Supported) typed response to the set
of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody505 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody505 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response505 a : responseCodes)
addResponseBody505 =
  addResponseBody status505

{- | Appends an HTTP 505 (HTTP Version Not Supported) Fleece response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema505 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema505 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response505 a : responseCodes)
addResponseSchema505 =
  addResponseSchema status505

{- | Appends an HTTP 505 (HTTP Version Not Supported) document response to the
set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument505
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument505 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response505 Document : responseCodes)
addResponseDocument505 =
  addResponseDocument status505

{- | Appends an HTTP 511 (Network Authentication Required) typed response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseBody511 myContentType myTypeEncoderFn
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseBody511 ::
  forall a responseCodes.
  -- | The MIME type, as a 'BS.ByteString'.
  ContentType ->
  {- | The encoder function, which takes a type @a@ and encodes it to a
  'LBS.ByteString'.
  -}
  (a -> LBS.ByteString) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response511 a : responseCodes)
addResponseBody511 =
  addResponseBody status511

{- | Appends an HTTP 511 (Network Authentication Required) Fleece response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseSchema511 myTypeSchema
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseSchema511 ::
  forall a responseCodes.
  (forall t. FC.Fleece t => FC.Schema t a) ->
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response511 a : responseCodes)
addResponseSchema511 =
  addResponseSchema status511

{- | Appends an HTTP 511 (Network Authentication Required) document response to
the set of possible responses.

/Example usage/:

@
responseBodies
  . addResponseDocument511
  $ noResponseBodies
@

@since 0.1.0
-}
addResponseDocument511 ::
  forall responseCodes.
  ResponseBodiesBuilder responseCodes ->
  ResponseBodiesBuilder (Response511 Document : responseCodes)
addResponseDocument511 =
  addResponseDocument status511

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

This type alias is primarily used to define more specific response functions
such as 'Return200' or 'Return404', which fix the @code@ to particular HTTP
status codes. Those synonyms, in turn, give you a convenient way to ensure that
any function returning them lines up exactly with the response codes in the
'ResponseBodiesBuilder'.

@since 0.1.0
-}
type ReturnType returnType index m responseCodes code =
  ( (returnType, HTTP.ResponseHeaders) ~ S.TagType code responseCodes
  , index ~ S.TagIndex code responseCodes
  , (returnType, HTTP.ResponseHeaders) ~ S.TypeAtIndex index (S.TaggedTypes responseCodes)
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

{- | A type synonym representing a monadic action returning an HTTP 100
(Continue) no-content response.

@since 0.1.0
-}
type Return100 index m responseCodes =
  ReturnType NoContent index m responseCodes "100"

{- | A type synonym representing a monadic action returning an HTTP 101
(Switching Protocols) no-content response.

@since 0.1.0
-}
type Return101 index m responseCodes =
  ReturnType NoContent index m responseCodes "101"

{- | A type synonym representing a monadic action returning an HTTP 200 (OK)
typed response.

@since 0.1.0
-}
type Return200 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "200"

{- | A type synonym representing a monadic action returning an HTTP 201
(Created) typed response.

@since 0.1.0
-}
type Return201 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "201"

{- | A type synonym representing a monadic action returning an HTTP 202
(Accepted) typed response.

@since 0.1.0
-}
type Return202 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "202"

{- | A type synonym representing a monadic action returning an HTTP 203
(Non-Authoritative Information) typed response.

@since 0.1.0
-}
type Return203 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "203"

{- | A type synonym representing a monadic action returning an HTTP 204 (No
Content) no-content response.

@since 0.1.0
-}
type Return204 index m responseCodes =
  ReturnType NoContent index m responseCodes "204"

{- | A type synonym representing a monadic action returning an HTTP 205 (Reset
Content) no-content response.

@since 0.1.0
-}
type Return205 index m responseCodes =
  ReturnType NoContent index m responseCodes "205"

{- | A type synonym representing a monadic action returning an HTTP 206
(Partial Content) typed response.

@since 0.1.0
-}
type Return206 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "206"

{- | A type synonym representing a monadic action returning an HTTP 300
(Multiple Choices) typed response.

@since 0.1.0
-}
type Return300 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "300"

{- | A type synonym representing a monadic action returning an HTTP 301 (Moved
Permanently) typed response.

@since 0.1.0
-}
type Return301 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "301"

{- | A type synonym representing a monadic action returning an HTTP 302 (Found)
typed response.

@since 0.1.0
-}
type Return302 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "302"

{- | A type synonym representing a monadic action returning an HTTP 303 (See
Other) typed response.

@since 0.1.0
-}
type Return303 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "303"

{- | A type synonym representing a monadic action returning an HTTP 304 (Not
Modified) no-content response.

@since 0.1.0
-}
type Return304 index m responseCodes =
  ReturnType NoContent index m responseCodes "304"

{- | A type synonym representing a monadic action returning an HTTP 305 (Use
Proxy) typed response.

@since 0.1.0
-}
type Return305 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "305"

{- | A type synonym representing a monadic action returning an HTTP 307
(Temporary Redirect) typed response.

@since 0.1.0
-}
type Return307 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "307"

{- | A type synonym representing a monadic action returning an HTTP 308
(Permanent Redirect) typed response.

@since 0.1.0
-}
type Return308 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "308"

{- | A type synonym representing a monadic action returning an HTTP 400 (Bad
Request) typed response.

@since 0.1.0
-}
type Return400 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "400"

{- | A type synonym representing a monadic action returning an HTTP 401
(Unauthorized) typed response.

@since 0.1.0
-}
type Return401 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "401"

{- | A type synonym representing a monadic action returning an HTTP 402
(Payment Required) typed response.

@since 0.1.0
-}
type Return402 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "402"

{- | A type synonym representing a monadic action returning an HTTP 403
(Forbidden) typed response.

@since 0.1.0
-}
type Return403 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "403"

{- | A type synonym representing a monadic action returning an HTTP 404 (Not
Found) typed response.

@since 0.1.0
-}
type Return404 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "404"

{- | A type synonym representing a monadic action returning an HTTP 405 (Method
Not Allowed) typed response.

@since 0.1.0
-}
type Return405 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "405"

{- | A type synonym representing a monadic action returning an HTTP 406 (Not
Acceptable) typed response.

@since 0.1.0
-}
type Return406 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "406"

{- | A type synonym representing a monadic action returning an HTTP 407 (Proxy
Authentication Required) typed response.

@since 0.1.0
-}
type Return407 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "407"

{- | A type synonym representing a monadic action returning an HTTP 408
(Request Timeout) typed response.

@since 0.1.0
-}
type Return408 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "408"

{- | A type synonym representing a monadic action returning an HTTP 409
(Conflict) typed response.

@since 0.1.0
-}
type Return409 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "409"

{- | A type synonym representing a monadic action returning an HTTP 410 (Gone)
typed response.

@since 0.1.0
-}
type Return410 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "410"

{- | A type synonym representing a monadic action returning an HTTP 411 (Length
Required) typed response.

@since 0.1.0
-}
type Return411 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "411"

{- | A type synonym representing a monadic action returning an HTTP 412
(Precondition Failed) typed response.

@since 0.1.0
-}
type Return412 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "412"

{- | A type synonym representing a monadic action returning an HTTP 413
(Payload Too Large) typed response.

@since 0.1.0
-}
type Return413 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "413"

{- | A type synonym representing a monadic action returning an HTTP 414 (URI
Too Long) typed response.

@since 0.1.0
-}
type Return414 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "414"

{- | A type synonym representing a monadic action returning an HTTP 415
(Unsupported Media Type) typed response.

@since 0.1.0
-}
type Return415 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "415"

{- | A type synonym representing a monadic action returning an HTTP 416 (Range
Not Satisfiable) typed response.

@since 0.1.0
-}
type Return416 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "416"

{- | A type synonym representing a monadic action returning an HTTP 417
(Expectation Failed) typed response.

@since 0.1.0
-}
type Return417 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "417"

{- | A type synonym representing a monadic action returning an HTTP 418 (I'm a
Teapot) typed response.

@since 0.1.0
-}
type Return418 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "418"

{- | A type synonym representing a monadic action returning an HTTP 422
(Unprocessable Entity) typed response.

@since 0.1.0
-}
type Return422 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "422"

{- | A type synonym representing a monadic action returning an HTTP 428
(Precondition Required) typed response.

@since 0.1.0
-}
type Return428 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "428"

{- | A type synonym representing a monadic action returning an HTTP 429 (Too
Many Requests) typed response.

@since 0.1.0
-}
type Return429 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "429"

{- | A type synonym representing a monadic action returning an HTTP 431
(Request Header Fields Too Large) typed response.

@since 0.1.0
-}
type Return431 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "431"

{- | A type synonym representing a monadic action returning an HTTP 500
(Internal Server Error) typed response.

@since 0.1.0
-}
type Return500 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "500"

{- | A type synonym representing a monadic action returning an HTTP 501 (Not
Implemented) typed response.

@since 0.1.0
-}
type Return501 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "501"

{- | A type synonym representing a monadic action returning an HTTP 502 (Bad
Gateway) typed response.

@since 0.1.0
-}
type Return502 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "502"

{- | A type synonym representing a monadic action returning an HTTP 503
(Service Unavailable) typed response.

@since 0.1.0
-}
type Return503 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "503"

{- | A type synonym representing a monadic action returning an HTTP 504
(Gateway Timeout) typed response.

@since 0.1.0
-}
type Return504 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "504"

{- | A type synonym representing a monadic action returning an HTTP 505 (HTTP
Version Not Supported) typed response.

@since 0.1.0
-}
type Return505 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "505"

{- | A type synonym representing a monadic action returning an HTTP 511
(Network Authentication Required) typed response.

@since 0.1.0
-}
type Return511 returnType index m responseCodes =
  ReturnType returnType index m responseCodes "511"

{- | Returns an HTTP 100 (Continue) no-content response.

@since 0.1.0
-}
return100 :: Return100 index m responseCodes
return100 = returnResponse status100

{- | Returns an HTTP 100 (Continue) no-content response with additional
headers.

@since 0.1.0
-}
return100WithHeaders :: HTTP.ResponseHeaders -> Return100 index m responseCodes
return100WithHeaders =
  returnResponseWithHeaders status100

{- | Returns an HTTP 101 (Switching Protocols) no-content response.

@since 0.1.0
-}
return101 :: Return101 index m responseCodes
return101 = returnResponse status101

{- | Returns an HTTP 101 (Switching Protocols) no-content response with
additional headers.

@since 0.1.0
-}
return101WithHeaders :: HTTP.ResponseHeaders -> Return101 index m responseCodes
return101WithHeaders =
  returnResponseWithHeaders status101

{- | Returns an HTTP 200 (OK) typed response.

@since 0.1.0
-}
return200 :: Return200 returnType index m responseCodes
return200 = returnResponse status200

{- | Returns an HTTP 200 (OK) typed response with additional headers.

@since 0.1.0
-}
return200WithHeaders :: HTTP.ResponseHeaders -> Return200 returnType index m responseCodes
return200WithHeaders =
  returnResponseWithHeaders status200

{- | Returns an HTTP 201 (Created) typed response.

@since 0.1.0
-}
return201 :: Return201 returnType index m responseCodes
return201 = returnResponse status201

{- | Returns an HTTP 201 (Created) typed response with additional headers.

@since 0.1.0
-}
return201WithHeaders :: HTTP.ResponseHeaders -> Return201 returnType index m responseCodes
return201WithHeaders =
  returnResponseWithHeaders status201

{- | Returns an HTTP 202 (Accepted) typed response.

@since 0.1.0
-}
return202 :: Return202 returnType index m responseCodes
return202 = returnResponse status202

{- | Returns an HTTP 202 (Accepted) typed response with additional headers.

@since 0.1.0
-}
return202WithHeaders :: HTTP.ResponseHeaders -> Return202 returnType index m responseCodes
return202WithHeaders =
  returnResponseWithHeaders status202

{- | Returns an HTTP 203 (Non-Authoritative Information) typed response.

@since 0.1.0
-}
return203 :: Return203 returnType index m responseCodes
return203 = returnResponse status203

{- | Returns an HTTP 203 (Non-Authoritative Information) typed response with
additional headers.

@since 0.1.0
-}
return203WithHeaders :: HTTP.ResponseHeaders -> Return203 returnType index m responseCodes
return203WithHeaders =
  returnResponseWithHeaders status203

{- | Returns an HTTP 204 (No Content) no-content response.

@since 0.1.0
-}
return204 :: Return204 index m responseCodes
return204 = returnResponse status204

{- | Returns an HTTP 204 (No Content) no-content response with additional
headers.

@since 0.1.0
-}
return204WithHeaders :: HTTP.ResponseHeaders -> Return204 index m responseCodes
return204WithHeaders =
  returnResponseWithHeaders status204

{- | Returns an HTTP 205 (Reset Content) no-content response.

@since 0.1.0
-}
return205 :: Return205 index m responseCodes
return205 = returnResponse status205

{- | Returns an HTTP 205 (Reset Content) no-content response with additional
headers.

@since 0.1.0
-}
return205WithHeaders :: HTTP.ResponseHeaders -> Return205 index m responseCodes
return205WithHeaders =
  returnResponseWithHeaders status205

{- | Returns an HTTP 206 (Partial Content) typed response.

@since 0.1.0
-}
return206 :: Return206 returnType index m responseCodes
return206 = returnResponse status206

{- | Returns an HTTP 206 (Partial Content) typed response with additional
headers.

@since 0.1.0
-}
return206WithHeaders :: HTTP.ResponseHeaders -> Return206 returnType index m responseCodes
return206WithHeaders =
  returnResponseWithHeaders status206

{- | Returns an HTTP 300 (Multiple Choices) typed response.

@since 0.1.0
-}
return300 :: Return300 returnType index m responseCodes
return300 = returnResponse status300

{- | Returns an HTTP 300 (Multiple Choices) typed response with additional
headers.

@since 0.1.0
-}
return300WithHeaders :: HTTP.ResponseHeaders -> Return300 returnType index m responseCodes
return300WithHeaders =
  returnResponseWithHeaders status300

{- | Returns an HTTP 301 (Moved Permanently) typed response.

@since 0.1.0
-}
return301 :: Return301 returnType index m responseCodes
return301 = returnResponse status301

{- | Returns an HTTP 301 (Moved Permanently) typed response with additional
headers.

@since 0.1.0
-}
return301WithHeaders :: HTTP.ResponseHeaders -> Return301 returnType index m responseCodes
return301WithHeaders =
  returnResponseWithHeaders status301

{- | Returns an HTTP 302 (Found) typed response.

@since 0.1.0
-}
return302 :: Return302 returnType index m responseCodes
return302 = returnResponse status302

{- | Returns an HTTP 302 (Found) typed response with additional headers.

@since 0.1.0
-}
return302WithHeaders :: HTTP.ResponseHeaders -> Return302 returnType index m responseCodes
return302WithHeaders =
  returnResponseWithHeaders status302

{- | Returns an HTTP 303 (See Other) typed response.

@since 0.1.0
-}
return303 :: Return303 returnType index m responseCodes
return303 = returnResponse status303

{- | Returns an HTTP 303 (See Other) typed response with additional headers.

@since 0.1.0
-}
return303WithHeaders :: HTTP.ResponseHeaders -> Return303 returnType index m responseCodes
return303WithHeaders =
  returnResponseWithHeaders status303

{- | Returns an HTTP 304 (Not Modified) no-content response.

@since 0.1.0
-}
return304 :: Return304 index m responseCodes
return304 = returnResponse status304

{- | Returns an HTTP 304 (Not Modified) no-content response with additional
headers.

@since 0.1.0
-}
return304WithHeaders :: HTTP.ResponseHeaders -> Return304 index m responseCodes
return304WithHeaders =
  returnResponseWithHeaders status304

{- | Returns an HTTP 305 (Use Proxy) typed response.

@since 0.1.0
-}
return305 :: Return305 returnType index m responseCodes
return305 = returnResponse status305

{- | Returns an HTTP 305 (Use Proxy) typed response with additional headers.

@since 0.1.0
-}
return305WithHeaders :: HTTP.ResponseHeaders -> Return305 returnType index m responseCodes
return305WithHeaders =
  returnResponseWithHeaders status305

{- | Returns an HTTP 307 (Temporary Redirect) typed response.

@since 0.1.0
-}
return307 :: Return307 returnType index m responseCodes
return307 = returnResponse status307

{- | Returns an HTTP 307 (Temporary Redirect) typed response with additional
headers.

@since 0.1.0
-}
return307WithHeaders :: HTTP.ResponseHeaders -> Return307 returnType index m responseCodes
return307WithHeaders =
  returnResponseWithHeaders status307

{- | Returns an HTTP 308 (Permanent Redirect) typed response.

@since 0.1.0
-}
return308 :: Return308 returnType index m responseCodes
return308 = returnResponse status308

{- | Returns an HTTP 308 (Permanent Redirect) typed response with additional
headers.

@since 0.1.0
-}
return308WithHeaders :: HTTP.ResponseHeaders -> Return308 returnType index m responseCodes
return308WithHeaders =
  returnResponseWithHeaders status308

{- | Returns an HTTP 400 (Bad Request) typed response.

@since 0.1.0
-}
return400 :: Return400 returnType index m responseCodes
return400 = returnResponse status400

{- | Returns an HTTP 400 (Bad Request) typed response with additional headers.

@since 0.1.0
-}
return400WithHeaders :: HTTP.ResponseHeaders -> Return400 returnType index m responseCodes
return400WithHeaders =
  returnResponseWithHeaders status400

{- | Returns an HTTP 401 (Unauthorized) typed response.

@since 0.1.0
-}
return401 :: Return401 returnType index m responseCodes
return401 = returnResponse status401

{- | Returns an HTTP 401 (Unauthorized) typed response with additional headers.

@since 0.1.0
-}
return401WithHeaders :: HTTP.ResponseHeaders -> Return401 returnType index m responseCodes
return401WithHeaders =
  returnResponseWithHeaders status401

{- | Returns an HTTP 402 (Payment Required) typed response.

@since 0.1.0
-}
return402 :: Return402 returnType index m responseCodes
return402 = returnResponse status402

{- | Returns an HTTP 402 (Payment Required) typed response with additional
headers.

@since 0.1.0
-}
return402WithHeaders :: HTTP.ResponseHeaders -> Return402 returnType index m responseCodes
return402WithHeaders =
  returnResponseWithHeaders status402

{- | Returns an HTTP 403 (Forbidden) typed response.

@since 0.1.0
-}
return403 :: Return403 returnType index m responseCodes
return403 = returnResponse status403

{- | Returns an HTTP 403 (Forbidden) typed response with additional headers.

@since 0.1.0
-}
return403WithHeaders :: HTTP.ResponseHeaders -> Return403 returnType index m responseCodes
return403WithHeaders =
  returnResponseWithHeaders status403

{- | Returns an HTTP 404 (Not Found) typed response.

@since 0.1.0
-}
return404 :: Return404 returnType index m responseCodes
return404 = returnResponse status404

{- | Returns an HTTP 404 (Not Found) typed response with additional headers.

@since 0.1.0
-}
return404WithHeaders :: HTTP.ResponseHeaders -> Return404 returnType index m responseCodes
return404WithHeaders =
  returnResponseWithHeaders status404

{- | Returns an HTTP 405 (Method Not Allowed) typed response.

@since 0.1.0
-}
return405 :: Return405 returnType index m responseCodes
return405 = returnResponse status405

{- | Returns an HTTP 405 (Method Not Allowed) typed response with additional
headers.

@since 0.1.0
-}
return405WithHeaders :: HTTP.ResponseHeaders -> Return405 returnType index m responseCodes
return405WithHeaders =
  returnResponseWithHeaders status405

{- | Returns an HTTP 406 (Not Acceptable) typed response.

@since 0.1.0
-}
return406 :: Return406 returnType index m responseCodes
return406 = returnResponse status406

{- | Returns an HTTP 406 (Not Acceptable) typed response with additional
headers.

@since 0.1.0
-}
return406WithHeaders :: HTTP.ResponseHeaders -> Return406 returnType index m responseCodes
return406WithHeaders =
  returnResponseWithHeaders status406

{- | Returns an HTTP 407 (Proxy Authentication Required) typed response.

@since 0.1.0
-}
return407 :: Return407 returnType index m responseCodes
return407 = returnResponse status407

{- | Returns an HTTP 407 (Proxy Authentication Required) typed response with
additional headers.

@since 0.1.0
-}
return407WithHeaders :: HTTP.ResponseHeaders -> Return407 returnType index m responseCodes
return407WithHeaders =
  returnResponseWithHeaders status407

{- | Returns an HTTP 408 (Request Timeout) typed response.

@since 0.1.0
-}
return408 :: Return408 returnType index m responseCodes
return408 = returnResponse status408

{- | Returns an HTTP 408 (Request Timeout) typed response with additional
headers.

@since 0.1.0
-}
return408WithHeaders :: HTTP.ResponseHeaders -> Return408 returnType index m responseCodes
return408WithHeaders =
  returnResponseWithHeaders status408

{- | Returns an HTTP 409 (Conflict) typed response.

@since 0.1.0
-}
return409 :: Return409 returnType index m responseCodes
return409 = returnResponse status409

{- | Returns an HTTP 409 (Conflict) typed response with additional headers.

@since 0.1.0
-}
return409WithHeaders :: HTTP.ResponseHeaders -> Return409 returnType index m responseCodes
return409WithHeaders =
  returnResponseWithHeaders status409

{- | Returns an HTTP 410 (Gone) typed response.

@since 0.1.0
-}
return410 :: Return410 returnType index m responseCodes
return410 = returnResponse status410

{- | Returns an HTTP 410 (Gone) typed response with additional headers.

@since 0.1.0
-}
return410WithHeaders :: HTTP.ResponseHeaders -> Return410 returnType index m responseCodes
return410WithHeaders =
  returnResponseWithHeaders status410

{- | Returns an HTTP 411 (Length Required) typed response.

@since 0.1.0
-}
return411 :: Return411 returnType index m responseCodes
return411 = returnResponse status411

{- | Returns an HTTP 411 (Length Required) typed response with additional
headers.

@since 0.1.0
-}
return411WithHeaders :: HTTP.ResponseHeaders -> Return411 returnType index m responseCodes
return411WithHeaders =
  returnResponseWithHeaders status411

{- | Returns an HTTP 412 (Precondition Failed) typed response.

@since 0.1.0
-}
return412 :: Return412 returnType index m responseCodes
return412 = returnResponse status412

{- | Returns an HTTP 412 (Precondition Failed) typed response with additional
headers.

@since 0.1.0
-}
return412WithHeaders :: HTTP.ResponseHeaders -> Return412 returnType index m responseCodes
return412WithHeaders =
  returnResponseWithHeaders status412

{- | Returns an HTTP 413 (Payload Too Large) typed response.

@since 0.1.0
-}
return413 :: Return413 returnType index m responseCodes
return413 = returnResponse status413

{- | Returns an HTTP 413 (Payload Too Large) typed response with additional
headers.

@since 0.1.0
-}
return413WithHeaders :: HTTP.ResponseHeaders -> Return413 returnType index m responseCodes
return413WithHeaders =
  returnResponseWithHeaders status413

{- | Returns an HTTP 414 (URI Too Long) typed response.

@since 0.1.0
-}
return414 :: Return414 returnType index m responseCodes
return414 = returnResponse status414

{- | Returns an HTTP 414 (URI Too Long) typed response with additional headers.

@since 0.1.0
-}
return414WithHeaders :: HTTP.ResponseHeaders -> Return414 returnType index m responseCodes
return414WithHeaders =
  returnResponseWithHeaders status414

{- | Returns an HTTP 415 (Unsupported Media Type) typed response.

@since 0.1.0
-}
return415 :: Return415 returnType index m responseCodes
return415 = returnResponse status415

{- | Returns an HTTP 415 (Unsupported Media Type) typed response with
additional headers.

@since 0.1.0
-}
return415WithHeaders :: HTTP.ResponseHeaders -> Return415 returnType index m responseCodes
return415WithHeaders =
  returnResponseWithHeaders status415

{- | Returns an HTTP 416 (Range Not Satisfiable) typed response.

@since 0.1.0
-}
return416 :: Return416 returnType index m responseCodes
return416 = returnResponse status416

{- | Returns an HTTP 416 (Range Not Satisfiable) typed response with additional
headers.

@since 0.1.0
-}
return416WithHeaders :: HTTP.ResponseHeaders -> Return416 returnType index m responseCodes
return416WithHeaders =
  returnResponseWithHeaders status416

{- | Returns an HTTP 417 (Expectation Failed) typed response.

@since 0.1.0
-}
return417 :: Return417 returnType index m responseCodes
return417 = returnResponse status417

{- | Returns an HTTP 417 (Expectation Failed) typed response with additional
headers.

@since 0.1.0
-}
return417WithHeaders :: HTTP.ResponseHeaders -> Return417 returnType index m responseCodes
return417WithHeaders =
  returnResponseWithHeaders status417

{- | Returns an HTTP 418 (I'm a Teapot) typed response.

@since 0.1.0
-}
return418 :: Return418 returnType index m responseCodes
return418 = returnResponse status418

{- | Returns an HTTP 418 (I'm a Teapot) typed response with additional headers.

@since 0.1.0
-}
return418WithHeaders :: HTTP.ResponseHeaders -> Return418 returnType index m responseCodes
return418WithHeaders =
  returnResponseWithHeaders status418

{- | Returns an HTTP 422 (Unprocessable Entity) typed response.

@since 0.1.0
-}
return422 :: Return422 returnType index m responseCodes
return422 = returnResponse status422

{- | Returns an HTTP 422 (Unprocessable Entity) typed response with additional
headers.

@since 0.1.0
-}
return422WithHeaders :: HTTP.ResponseHeaders -> Return422 returnType index m responseCodes
return422WithHeaders =
  returnResponseWithHeaders status422

{- | Returns an HTTP 428 (Precondition Required) typed response.

@since 0.1.0
-}
return428 :: Return428 returnType index m responseCodes
return428 = returnResponse status428

{- | Returns an HTTP 428 (Precondition Required) typed response with additional
headers.

@since 0.1.0
-}
return428WithHeaders :: HTTP.ResponseHeaders -> Return428 returnType index m responseCodes
return428WithHeaders =
  returnResponseWithHeaders status428

{- | Returns an HTTP 429 (Too Many Requests) typed response.

@since 0.1.0
-}
return429 :: Return429 returnType index m responseCodes
return429 = returnResponse status429

{- | Returns an HTTP 429 (Too Many Requests) typed response with additional
headers.

@since 0.1.0
-}
return429WithHeaders :: HTTP.ResponseHeaders -> Return429 returnType index m responseCodes
return429WithHeaders =
  returnResponseWithHeaders status429

{- | Returns an HTTP 431 (Request Header Fields Too Large) typed response.

@since 0.1.0
-}
return431 :: Return431 returnType index m responseCodes
return431 = returnResponse status431

{- | Returns an HTTP 431 (Request Header Fields Too Large) typed response with
additional headers.

@since 0.1.0
-}
return431WithHeaders :: HTTP.ResponseHeaders -> Return431 returnType index m responseCodes
return431WithHeaders =
  returnResponseWithHeaders status431

{- | Returns an HTTP 500 (Internal Server Error) typed response.

@since 0.1.0
-}
return500 :: Return500 returnType index m responseCodes
return500 = returnResponse status500

{- | Returns an HTTP 500 (Internal Server Error) typed response with additional
headers.

@since 0.1.0
-}
return500WithHeaders :: HTTP.ResponseHeaders -> Return500 returnType index m responseCodes
return500WithHeaders =
  returnResponseWithHeaders status500

{- | Returns an HTTP 501 (Not Implemented) typed response.

@since 0.1.0
-}
return501 :: Return501 returnType index m responseCodes
return501 = returnResponse status501

{- | Returns an HTTP 501 (Not Implemented) typed response with additional
headers.

@since 0.1.0
-}
return501WithHeaders :: HTTP.ResponseHeaders -> Return501 returnType index m responseCodes
return501WithHeaders =
  returnResponseWithHeaders status501

{- | Returns an HTTP 502 (Bad Gateway) typed response.

@since 0.1.0
-}
return502 :: Return502 returnType index m responseCodes
return502 = returnResponse status502

{- | Returns an HTTP 502 (Bad Gateway) typed response with additional headers.

@since 0.1.0
-}
return502WithHeaders :: HTTP.ResponseHeaders -> Return502 returnType index m responseCodes
return502WithHeaders =
  returnResponseWithHeaders status502

{- | Returns an HTTP 503 (Service Unavailable) typed response.

@since 0.1.0
-}
return503 :: Return503 returnType index m responseCodes
return503 = returnResponse status503

{- | Returns an HTTP 503 (Service Unavailable) typed response with additional
headers.

@since 0.1.0
-}
return503WithHeaders :: HTTP.ResponseHeaders -> Return503 returnType index m responseCodes
return503WithHeaders =
  returnResponseWithHeaders status503

{- | Returns an HTTP 504 (Gateway Timeout) typed response.

@since 0.1.0
-}
return504 :: Return504 returnType index m responseCodes
return504 = returnResponse status504

{- | Returns an HTTP 504 (Gateway Timeout) typed response with additional
headers.

@since 0.1.0
-}
return504WithHeaders :: HTTP.ResponseHeaders -> Return504 returnType index m responseCodes
return504WithHeaders =
  returnResponseWithHeaders status504

{- | Returns an HTTP 505 (HTTP Version Not Supported) typed response.

@since 0.1.0
-}
return505 :: Return505 returnType index m responseCodes
return505 = returnResponse status505

{- | Returns an HTTP 505 (HTTP Version Not Supported) typed response with
additional headers.

@since 0.1.0
-}
return505WithHeaders :: HTTP.ResponseHeaders -> Return505 returnType index m responseCodes
return505WithHeaders =
  returnResponseWithHeaders status505

{- | Returns an HTTP 511 (Network Authentication Required) typed response.

@since 0.1.0
-}
return511 :: Return511 returnType index m responseCodes
return511 = returnResponse status511

{- | Returns an HTTP 511 (Network Authentication Required) typed response with
additional headers.

@since 0.1.0
-}
return511WithHeaders :: HTTP.ResponseHeaders -> Return511 returnType index m responseCodes
return511WithHeaders =
  returnResponseWithHeaders status511
