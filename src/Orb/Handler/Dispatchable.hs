{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Orb.Handler.Dispatchable
  ( Dispatchable (..)
  ) where

import Control.Exception.Safe qualified as Safe
import Control.Monad.IO.Class qualified as MIO
import GHC.TypeLits (KnownNat)
import Network.Wai qualified as Wai
import Shrubbery qualified as S

import Orb.Handler.Handler qualified as Handler
import Orb.Handler.PermissionAction qualified as PA
import Orb.HasLogger qualified as HasLogger
import Orb.HasRequest qualified as HasRequest
import Orb.HasRespond qualified as HasRespond

class Dispatchable m a where
  dispatch :: a -> m Wai.ResponseReceived

instance
  {-# OVERLAPPABLE #-}
  ( HasLogger.HasLogger m
  , HasRequest.HasRequest m
  , HasRespond.HasRespond m
  , MIO.MonadIO m
  , Handler.HasHandler route
  , PA.PermissionActionMonad (Handler.HandlerPermissionAction route) ~ m
  , Safe.MonadCatch m
  ) =>
  Dispatchable m route
  where
  dispatch =
    Handler.runHandler Handler.routeHandler

instance
  (DispatchableBranches m routes, KnownNat (S.Length routes)) =>
  Dispatchable m (S.Union routes)
  where
  dispatch =
    S.dissectUnion . S.branchBuild $ dispatchBranchBuilder

class DispatchableBranches m tags where
  dispatchBranchBuilder :: S.BranchBuilder tags (m Wai.ResponseReceived)

instance DispatchableBranches m '[] where
  dispatchBranchBuilder =
    S.branchEnd

instance
  (Dispatchable m route, DispatchableBranches m routes) =>
  DispatchableBranches m (route : routes)
  where
  dispatchBranchBuilder =
    S.branch @route dispatch dispatchBranchBuilder
