{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Orb.Handler.PermissionError
  ( PermissionError (..)
  ) where

import Data.Kind (Constraint, Type)
import Shrubbery qualified as S

{- |
  This class allows each kind of permission error to determine what response codes
  it produces such that each handler can faithfully document only the permission
  error codes that it will actually produce. While it possible for handlers to
  define their own 'PermissionError' types if necessary, usually you will just
  use one of the existing instances of this type found in the 'Merlin.Permission'
  directory (e.g. 'StandardPermissionError')
-}
class PermissionError err where
  -- |
  --     'PermissionErrorMonad' is an associated type that specifies the monad
  --     in which the permission error handling operates.
  type PermissionErrorMonad err :: Type -> Type

  -- |
  --     'PermissionErrorConstraints' is an associated type that allows each permission
  --     error instance to define exactly which HTTP status codes it produces when
  --     'returnPermissionError' is used to handle an error. Usually this constraints
  --     will be defined using helpers such as 'R.Has401Response' to indicate that the
  --     set of all response codes returned by the handler includes those that are
  --     produced by the 'PermissionError'
  type PermissionErrorConstraints err (tags :: [S.Tag]) :: Constraint

  -- |
  --     'returnPermissionError' is called as part of running a Handler when the
  --     permission check for the handler returns an error. It constructs the
  --     response an instance of the Handler's response type as appropriate for
  --     the error value passed to it, generally using helpers such as 'H.return401'.
  returnPermissionError ::
    PermissionErrorConstraints err tags =>
    err ->
    PermissionErrorMonad err (S.TaggedUnion tags)
