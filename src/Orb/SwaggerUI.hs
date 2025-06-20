{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Orb.SwaggerUI
  ( SwaggerUIRoute (SwaggerUIRoute, swaggerFilePath)
  , swaggerUIRoutes
  ) where

import Beeline.Routing ((/+))
import Beeline.Routing qualified as R
import Control.Monad.IO.Class qualified as MIO
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed qualified as FileEmbed
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

import Orb.Handler qualified as Handler
import Orb.HasRespond qualified as HasRespond
import Orb.Response qualified as Response

newtype SwaggerUIRoute = SwaggerUIRoute
  { swaggerFilePath :: T.Text
  }

instance (HasRespond.HasRespond m, MIO.MonadIO m) => Handler.Dispatchable m SwaggerUIRoute where
  dispatch (SwaggerUIRoute path) =
    Response.respondWith $
      case lookup (T.unpack path) swaggerUIFiles of
        Just bytes ->
          Wai.responseLBS
            HTTP.status200
            [] -- todo set content type
            (LBS.fromStrict bytes)
        Nothing ->
          Wai.responseLBS
            HTTP.status404
            [] -- todo set content type
            ("SwaggerUI: File Not Found" :: LBS.ByteString)

swaggerUIRoutes :: R.Router r => r SwaggerUIRoute
swaggerUIRoutes =
  R.get (R.make SwaggerUIRoute /+ R.Param fileNameParam swaggerFilePath)

fileNameParam :: R.ParameterDefinition T.Text
fileNameParam =
  R.textParam "fileName"

swaggerUIFiles :: [(FilePath, BS.ByteString)]
swaggerUIFiles =
  $(FileEmbed.embedDir "src/Orb/SwaggerUI/swagger-ui-dist-5.25.2")
