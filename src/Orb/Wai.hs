{-# LANGUAGE FlexibleContexts #-}

module Orb.Wai
  ( runOrb
  , runOrbSettings
  , defaultOrbApp
  , defaultDispatcher
  , defaultHandleNotFound
  , OrbApp (OrbApp, router, dispatcher, handleNotFound)
  , orbAppToWai
  ) where

import Beeline.Routing qualified as R
import Data.ByteString.Lazy.Char8 qualified as LB8
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Orb.Handler.Dispatchable qualified as Dispatchable

data OrbApp route = OrbApp
  { router :: R.RouteRecognizer route
  , dispatcher :: route -> Wai.Application
  , handleNotFound :: Wai.Application
  }

defaultOrbApp ::
  Dispatchable.Dispatchable IO route =>
  R.RouteRecognizer route ->
  OrbApp route
defaultOrbApp routes =
  OrbApp
    { router = routes
    , dispatcher = defaultDispatcher
    , handleNotFound = defaultHandleNotFound
    }

defaultDispatcher ::
  Dispatchable.Dispatchable IO route =>
  route ->
  Wai.Application
defaultDispatcher route _request _respond =
  -- TODO this default dispatch is basically broken -- it can never be used
  -- with a real application because the request body cannot be parsed
  Dispatchable.dispatch route

defaultHandleNotFound :: Wai.Application
defaultHandleNotFound _request respond =
  respond notFoundResponse

notFoundResponse :: Wai.Response
notFoundResponse =
  Wai.responseLBS HTTP.status404 [] (LB8.pack "Not Found")

orbAppToWai ::
  OrbApp route ->
  Wai.Application
orbAppToWai app request respond =
  case HTTP.parseMethod $ Wai.requestMethod request of
    Left _err -> handleNotFound app request respond
    Right method ->
      case R.recognizeRoute (router app) method (Wai.pathInfo request) of
        Left _err -> handleNotFound app request respond
        Right route -> dispatcher app route request respond

runOrb :: OrbApp route -> IO ()
runOrb =
  runOrbSettings Warp.defaultSettings

runOrbSettings :: Warp.Settings -> OrbApp route -> IO ()
runOrbSettings settings =
  Warp.runSettings settings . orbAppToWai
