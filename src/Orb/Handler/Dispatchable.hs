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

import GHC.TypeLits (KnownNat)
import Network.Wai qualified as Wai
import Shrubbery qualified as S
import UnliftIO qualified

import Orb.Handler.Handler qualified as Handler
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
  , UnliftIO.MonadUnliftIO m
  , Handler.HasHandler route
  , Handler.HandlerMonad route ~ m
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
