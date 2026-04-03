{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Fixtures.SchemaDescriptions
  ( schemaDescriptionsOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Shrubbery qualified

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

schemaDescriptionsOpenApiRouter ::
  Orb.OpenApiProvider r =>
  r (Shrubbery.Union '[SchemaDescriptions])
schemaDescriptionsOpenApiRouter =
  Orb.provideOpenApi "schema-descriptions" $
    R.routeList $
      (Orb.get (R.make SchemaDescriptions /- T.pack "schema-descriptions"))
        /: R.emptyRoutes

newtype DescribedObject
  = DescribedObject
  { describedObjectContent :: T.Text
  }

describedObjectSchema :: FC.Fleece t => FC.Schema t DescribedObject
describedObjectSchema =
  let
    objectDescription = T.pack "This is the description for DescribedObject."
    fieldDescription = T.pack "This is the description for the content field."
  in
    FC.describe objectDescription $
      FC.object $
        FC.constructor DescribedObject
          #+ FC.required "content" describedObjectContent (FC.describe fieldDescription FC.text)

data SchemaDescriptions = SchemaDescriptions

instance Orb.HasHandler SchemaDescriptions where
  type HandlerResponses SchemaDescriptions = SchemaDescriptionsResponses
  type HandlerPermissionAction SchemaDescriptions = NoPermissions
  type HandlerMonad SchemaDescriptions = TDM.TestDispatchM
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "SchemaDescriptionsHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.requestQuery = Orb.EmptyRequestQuery
      , Orb.requestHeaders = Orb.EmptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 describedObjectSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.return200 . DescribedObject $ T.pack "Described content."
      }

type SchemaDescriptionsResponses =
  [ Orb.Response200 DescribedObject
  , Orb.Response500 Orb.InternalServerError
  ]
