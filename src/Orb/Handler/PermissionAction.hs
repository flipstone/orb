{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Orb.Handler.PermissionAction
  ( PermissionAction (..)
  ) where

import Data.Kind (Type)

import Orb.Handler.PermissionError (PermissionError)

{- |
  'PermissionAction' allows each handler to define what logical action the user is
  attempting to take from a permissions perspective so that authorization can be
  handled in a suitably generic and re-usable way. It some cases it may be possible
  for handlers to share the same 'PermissionAction' type (e.g. see 'LoginAction'
  that in used both to create auth tokens and refresh them), but more often it
  makes sense for each handler to have its own action type as a discreet type that
  can have its own permissions checking logic. In trivial cases it may be possible
  to re-use the route type from the handler as the type with a 'PermissionAction'
  instances, but in other cases it information from both the route and the request
  body may need to be made available to the permissions check.

  Each action type can produce a distinct 'PermissionActionResult' and
  'PermissionActionError' type. The result type is ultimately passed to the request
  handling function if the permission check succeeds. If the 'checkPermissionAction'
  returns the error type then this is used to formulate a response to the client
  via the error types 'PermissionError' type and the request handler is never
  executed.
-}
class PermissionError (PermissionActionError action) => PermissionAction action where
  -- |
  --     'PermissionActionMonad' is an associated type that indicates the monad in which
  --     the permission check will be executed.
  type PermissionActionMonad action :: Type -> Type

  -- |
  --     'PermissionActionResult' is an associated type that indicates what type of value
  --     the 'action' produces as a result of a successful permission check. The value
  --     produced by the permission check will ultimately be passed to the request handler
  --     so that it can make use of information gained during the permission check.
  type PermissionActionResult action :: Type

  -- |
  --     'PermissionActionError' is an associated type that indicates what type of error
  --     the 'action' produces when a permission check fails. Allowing different instance
  --     of 'PermissionAction' to produce different errors allows each handler to document
  --     exactly the permission errors that it might actually produce rather than applying
  --     all permission errors to all handlers.
  type PermissionActionError action :: Type

  -- |
  --     'checkPermissionAction' will be called as part of running a 'Handler' to handle
  --     an incoming request. If the check succeeds then the result of the check will
  --     be passed on to the request handler. If the check fails then an error will be
  --     returned to the client.
  checkPermissionAction ::
    action ->
    PermissionActionMonad action (Either (PermissionActionError action) (PermissionActionResult action))
