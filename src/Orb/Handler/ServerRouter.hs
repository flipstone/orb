{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Orb.Handler.ServerRouter
  ( ServerRouter (..)
  , connect
  , delete
  , get
  , head
  , options
  , patch
  , post
  , put
  , trace
  ) where

import Beeline.Routing qualified as R
import Network.HTTP.Types qualified as HTTPTypes
import Prelude hiding (head)

import Orb.Handler.Handler (Handler, HasHandler, routeHandler)

class R.Router r => ServerRouter r where
  methodHandler ::
    HTTPTypes.StdMethod ->
    Handler route ->
    R.Builder r route route ->
    r route

instance ServerRouter R.RouteRecognizer where
  methodHandler stdMethod _handler =
    R.method stdMethod

connect ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
connect =
  methodHandler HTTPTypes.CONNECT (routeHandler @route)

delete ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
delete =
  methodHandler HTTPTypes.DELETE (routeHandler @route)

get ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
get =
  methodHandler HTTPTypes.GET (routeHandler @route)

head ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
head =
  methodHandler HTTPTypes.HEAD (routeHandler @route)

options ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
options =
  methodHandler HTTPTypes.OPTIONS (routeHandler @route)

patch ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
patch =
  methodHandler HTTPTypes.PATCH (routeHandler @route)

post ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
post =
  methodHandler HTTPTypes.POST (routeHandler @route)

put ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
put =
  methodHandler HTTPTypes.PUT (routeHandler @route)

trace ::
  forall route r.
  (ServerRouter r, HasHandler route) =>
  R.Builder r route route ->
  r route
trace =
  methodHandler HTTPTypes.TRACE (routeHandler @route)
