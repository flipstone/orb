{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.SchemaBounds
  ( SchemaBounds (..)
  , schemaBoundsOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

schemaBoundsOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[SchemaBounds])
schemaBoundsOpenApiRouter =
  Orb.provideOpenApi "schema-bounds"
    . R.routeList
    $ Orb.get (R.make SchemaBounds /- "test" /- "schema_bounds")
      /: R.emptyRoutes

data SchemaBounds = SchemaBounds

instance Orb.HasHandler SchemaBounds where
  type HandlerResponses SchemaBounds = Responses
  type HandlerPermissionAction SchemaBounds = NoPermissions
  type HandlerMonad SchemaBounds = TDM.TestDispatchM
  routeHandler = handler

type Responses =
  [ Orb.Response200 SchemaBoundsResponse
  , Orb.Response500 Orb.InternalServerError
  ]

data SchemaBoundsResponse = SchemaBoundsResponse
  { boundedTextField :: T.Text
  , boundedListField :: [T.Text]
  , boundedNumberField :: Int
  }

schemaBoundsResponseSchema :: FC.Fleece t => FC.Schema t SchemaBoundsResponse
schemaBoundsResponseSchema =
  FC.object $
    FC.constructor SchemaBoundsResponse
      #+ FC.required "bounded_text" boundedTextField (FC.maxLength 100 $ FC.minLength 1 FC.text)
      #+ FC.required "bounded_list" boundedListField (FC.maxItems 50 $ FC.minItems 1 (FC.list FC.text))
      #+ FC.required "bounded_number" boundedNumberField (FC.maximum 1000 $ FC.minimum 0 FC.int)

handler :: Orb.Handler SchemaBounds
handler =
  Orb.Handler
    { Orb.handlerId = "schemaBounds"
    , Orb.requestBody = Orb.EmptyRequestBody
    , Orb.requestQuery = Orb.EmptyRequestQuery
    , Orb.requestHeaders = Orb.EmptyRequestHeaders
    , Orb.handlerResponseBodies =
        Orb.responseBodies
          . Orb.addResponseSchema200 schemaBoundsResponseSchema
          . Orb.addResponseSchema500 Orb.internalServerErrorSchema
          $ Orb.noResponseBodies
    , Orb.mkPermissionAction =
        \_request -> NoPermissions
    , Orb.handleRequest =
        \_request () ->
          Orb.return200
            SchemaBoundsResponse
              { boundedTextField = "hello"
              , boundedListField = ["item"]
              , boundedNumberField = 42
              }
    }
