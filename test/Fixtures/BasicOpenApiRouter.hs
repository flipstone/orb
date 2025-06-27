{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixtures.BasicOpenApiRouter
  ( basicOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Shrubbery qualified as S

import Fixtures.GetWithHeaders (GetWithHeaders (..))
import Fixtures.GetWithQuery (GetWithQuery (..))
import Fixtures.SimpleGet (SimpleGet (..))
import Orb qualified

basicOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union [SimpleGet, GetWithHeaders, GetWithQuery])
basicOpenApiRouter =
  Orb.provideOpenApi "basic-open-api"
    . R.routeList
    $ Orb.provideOpenApi "just-route-1" (Orb.get (R.make SimpleGet /- "test" /- "simple_get"))
      /: Orb.get (R.make GetWithHeaders /- "test" /- "get_with_headers")
      /: Orb.get (R.make GetWithQuery /- "test" /- "get_with_query")
      /: R.emptyRoutes
