{-# LANGUAGE TypeFamilies #-}

module Fixtures.NoPermissions
  ( NoPermissions (..)
  , NoError
  ) where

import Data.Void qualified as Void

import Orb qualified
import TestDispatchM qualified as TDM

data NoPermissions
  = NoPermissions

newtype NoError = NoError Void.Void

instance Orb.PermissionAction NoPermissions where
  type PermissionActionMonad NoPermissions = TDM.TestDispatchM
  type PermissionActionError NoPermissions = NoError
  type PermissionActionResult NoPermissions = ()

  checkPermissionAction _ =
    pure (Right ())

instance Orb.PermissionError NoError where
  type PermissionErrorConstraints NoError _tags = ()
  type PermissionErrorMonad NoError = TDM.TestDispatchM

  returnPermissionError (NoError void) =
    Void.absurd void
