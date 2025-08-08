{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.OpenApiSubset
  ( openApiSubsetRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Shrubbery qualified as S

import Fixtures.SimpleGet (SimpleGet (SimpleGet))
import Orb qualified

openApiSubsetRouter :: Orb.OpenApiProvider r => r (S.Union '[SimpleGet, SimpleGet])
openApiSubsetRouter =
  Orb.provideOpenApi "whole-open-api"
    . R.routeList
    $ Orb.get (R.make SimpleGet /- "test" /- "route-1")
      /: Orb.provideOpenApi "open-api-subset" (Orb.get (R.make SimpleGet /- "test" /- "route-2"))
      /: R.emptyRoutes
