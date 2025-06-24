{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Orb.SwaggerUI
  ( swaggerUIRoutes
  , SwaggerUIRoute (SwaggerUIRoute, swaggerOpenApis, swaggerApiLabel, swaggerUIPath)
  , dispatchSwaggerUIRoute
  , SwaggerUIPath
  , SwaggerUIIndexRedirect (SwaggerUIIndexRedirect)
  , SwaggerUIIndex (SwaggerUIIndex)
  , SwaggerUIOpenApi (SwaggerUIOpenApi)
  , SwaggerUIResource (SwaggerUIResource, swaggerUIResourcePath)
  ) where

import Beeline.Routing ((/+), (/-), (/:), (/>))
import Beeline.Routing qualified as R
import Control.Monad.IO.Class qualified as MIO
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.FileEmbed qualified as FileEmbed
import Data.Map qualified as Map
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Shrubbery qualified as S
import System.FilePath qualified as FilePath

import Orb.Handler qualified as Handler
import Orb.HasRequest qualified as HasRequest
import Orb.HasRespond qualified as HasRespond
import Orb.OpenApi qualified as OrbOpenApi
import Orb.Response qualified as Response

{- |
  These routes can be included in your application routes to provide an
  auto-generate SwaggerUI for any OpenAPI specs defined by the router passed
  in. The provided router should have the root-level routing of the APIs so
  that they can be invoked successfully via the paths found in the generated
  OpenAPI specs. The 'SwaggerUIRoute' type has a 'Dispatchable' instance, so
  requests can be handled via 'Orb.dispatch', which will be done automatically
  if the routes are in a Shrubbery union using Orbs automatic dispatching for
  unions.

  A separate SwaggerUI will be offered for each of the APIs labeled via
  'OrbOpenApi.provideOpenApi' in the provided router. These can be accessed via
  @/<api-label>@ under whatever path you choose to include 'swaggerUIRoutes'
  within your application's routes.
-}
swaggerUIRoutes :: R.Router r => OrbOpenApi.OpenApiRouter a -> r SwaggerUIRoute
swaggerUIRoutes openApiRouter =
  let
    pathRouter =
      R.routeList $
        R.get (R.make SwaggerUIIndexRedirect)
          /: R.get (R.make SwaggerUIIndex /- "")
          /: R.get (R.make SwaggerUIOpenApi /- "open-api.json")
          /: R.get (R.make SwaggerUIResource /+ R.Param fileNameParam swaggerUIResourcePath)
          /: R.emptyRoutes
  in
    R.make (SwaggerUIRoute (OrbOpenApi.mkAllOpenApis openApiRouter))
      /+ R.Param apiLabelParam swaggerApiLabel
      /> R.Subrouter pathRouter swaggerUIPath

apiLabelParam :: R.ParameterDefinition T.Text
apiLabelParam =
  R.textParam "apiLabel"

fileNameParam :: R.ParameterDefinition T.Text
fileNameParam =
  R.textParam "fileName"

{- |
  A route type containing all the information necessary to serve a functioning
  SwaggerUI for the 'OpenApi.OpenApi' definitions contained in the map. Usually
  this map is constructed via 'OrbOpenApi.mkAllOpenApis', so this allows for
  the possibility of an error being returned by the function. Any such errors
  will be raised as internal server errors when the SwaggerUI is accessed via
  the browser.
-}
data SwaggerUIRoute = SwaggerUIRoute
  { swaggerOpenApis :: Either String (Map.Map String OpenApi.OpenApi)
  , swaggerApiLabel :: T.Text
  , swaggerUIPath :: SwaggerUIPath
  }

type SwaggerUIPath =
  S.Union
    [ SwaggerUIIndexRedirect
    , SwaggerUIIndex
    , SwaggerUIOpenApi
    , SwaggerUIResource
    ]

{- |
  The Swagger UI reliese on relative urls being resolved via the browser, so
  if a request is made to the index without a trailing slash, once will be
  added via an HTTP redirect.
-}
data SwaggerUIIndexRedirect = SwaggerUIIndexRedirect

{- |
  Serves the HTML to load and initialize the SwaggerUI
-}
data SwaggerUIIndex = SwaggerUIIndex

{- |
  Serves the JSON representation of an OpenApi spec for an API that has been
  labeled via 'OrbOpenApi.provideOpenApi'
-}
data SwaggerUIOpenApi = SwaggerUIOpenApi

{- |
  Serves one of a number of SwaggerUI resources files, such as javascript and
  css.
-}
newtype SwaggerUIResource = SwaggerUIResource
  { swaggerUIResourcePath :: T.Text
  }

instance
  (HasRespond.HasRespond m, HasRequest.HasRequest m, MIO.MonadIO m) =>
  Handler.Dispatchable m SwaggerUIRoute
  where
  dispatch = dispatchSwaggerUIRoute

{- |
  Handles a 'SwaggerUIRoute' request. You can call this directly if you
  need to exert more control over dispatching than is offered by the
  default automatic dispatching.
-}
dispatchSwaggerUIRoute ::
  (HasRespond.HasRespond m, HasRequest.HasRequest m, MIO.MonadIO m) =>
  SwaggerUIRoute ->
  m Wai.ResponseReceived
dispatchSwaggerUIRoute (SwaggerUIRoute errOrOpenApis apiLabel apiPath) =
  case errOrOpenApis of
    Left err ->
      Response.respondWith $
        Wai.responseLBS
          HTTP.status500
          [("Content-Type", "text/plain")]
          (LBS8.pack err)
    Right allOpenApis ->
      case Map.lookup (T.unpack apiLabel) allOpenApis of
        Nothing -> Response.respondWith notFoundResponse
        Just openApi -> dispatchSwaggerUIPath openApi apiPath

dispatchSwaggerUIPath ::
  (HasRespond.HasRespond m, HasRequest.HasRequest m, MIO.MonadIO m) =>
  OpenApi.OpenApi ->
  SwaggerUIPath ->
  m Wai.ResponseReceived
dispatchSwaggerUIPath openApi =
  S.dissect
    . S.branchBuild
    . S.branch @SwaggerUIIndexRedirect (const handleIndexRedirect)
    . S.branch @SwaggerUIIndex (const handleIndex)
    . S.branch @SwaggerUIOpenApi (const (handleOpenApi openApi))
    . S.branch @SwaggerUIResource handleSwaggerUIResource
    $ S.branchEnd

handleIndexRedirect ::
  (HasRespond.HasRespond m, HasRequest.HasRequest m, MIO.MonadIO m) =>
  m Wai.ResponseReceived
handleIndexRedirect = do
  request <- HasRequest.request
  Response.respondWith $
    Wai.responseLBS
      HTTP.status308
      [ ("Location", BS8.snoc (Wai.rawPathInfo request) '/')
      , ("Content-Type", "text/plain")
      ]
      ("Please use a trailing /" :: LBS.ByteString)

handleIndex :: (HasRespond.HasRespond m, MIO.MonadIO m) => m Wai.ResponseReceived
handleIndex =
  Response.respondWith $
    Wai.responseLBS
      HTTP.status200
      [("Content-Type", "text/html")]
      (LBS.fromStrict swaggerUIIndex)

handleOpenApi ::
  (HasRespond.HasRespond m, MIO.MonadIO m) =>
  OpenApi.OpenApi ->
  m Wai.ResponseReceived
handleOpenApi openApi =
  Response.respondWith $
    Wai.responseLBS
      HTTP.status200
      [("Content-Type", "application/json")]
      (AesonPretty.encodePretty openApi)

handleSwaggerUIResource ::
  (HasRespond.HasRespond m, MIO.MonadIO m) =>
  SwaggerUIResource ->
  m Wai.ResponseReceived
handleSwaggerUIResource (SwaggerUIResource path) =
  Response.respondWith $
    case path of
      "index.html" ->
        -- the dist directory of swagger-ui includes an
        -- index.html that we do not want to serve accidentally
        -- because that will just create confusion
        notFoundResponse
      _ ->
        let
          pathString =
            T.unpack path
        in
          case lookup pathString swaggerUIFiles of
            Nothing -> notFoundResponse
            Just bytes ->
              Wai.responseLBS
                HTTP.status200
                [("Content-Type", contentTypeForFile pathString)]
                (LBS.fromStrict bytes)

contentTypeForFile :: FilePath -> BS.ByteString
contentTypeForFile path =
  case FilePath.takeExtension path of
    ".js" -> "text/javascript"
    ".css" -> "text/css"
    ".png" -> "image/png"
    ".html" -> "text/html"
    _ -> "application/octet-stream"

notFoundResponse :: Wai.Response
notFoundResponse =
  Wai.responseLBS
    HTTP.status404
    [("Content-Type", "text/plain")]
    ("SwaggerUI: File Not Found" :: LBS.ByteString)

swaggerUIIndex :: BS.ByteString
swaggerUIIndex =
  $(FileEmbed.embedFile "src/Orb/SwaggerUI/swagger-ui-index.html")

swaggerUIFiles :: [(FilePath, BS.ByteString)]
swaggerUIFiles =
  $(FileEmbed.embedDir "src/Orb/SwaggerUI/swagger-ui-dist-5.25.2")
