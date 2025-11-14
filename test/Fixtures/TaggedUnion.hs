{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fixtures.TaggedUnion
  ( TaggedUnion (..)
  , taggedUnionOpenApiRouter
  ) where

import Beeline.Routing ((/-), (/:))
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Fleece.Core ((#+), (#@))
import Fleece.Core qualified as FC
import Shrubbery (type (@=))
import Shrubbery qualified as S

import Fixtures.NoPermissions (NoPermissions (NoPermissions))
import Orb qualified
import TestDispatchM qualified as TDM

taggedUnionOpenApiRouter :: Orb.OpenApiProvider r => r (S.Union '[TaggedUnion])
taggedUnionOpenApiRouter =
  Orb.provideOpenApi "tagged-union"
    . R.routeList
    $ (Orb.get (R.make TaggedUnion /- "tagged-union"))
      /: R.emptyRoutes

data TaggedUnion = TaggedUnion

instance Orb.HasHandler TaggedUnion where
  type HandlerResponses TaggedUnion = TaggedUnionResponses
  type HandlerPermissionAction TaggedUnion = NoPermissions
  type HandlerMonad TaggedUnion = TDM.TestDispatchM
  routeHandler =
    Orb.Handler
      { Orb.handlerId = "TaggedUnionHandler"
      , Orb.requestBody = Orb.EmptyRequestBody
      , Orb.requestQuery = Orb.EmptyRequestQuery
      , Orb.requestHeaders = Orb.EmptyRequestHeaders
      , Orb.handlerResponseBodies =
          Orb.responseBodies
            . Orb.addResponseSchema200 unionResponseSchema
            . Orb.addResponseSchema500 Orb.internalServerErrorSchema
            $ Orb.noResponseBodies
      , Orb.mkPermissionAction =
          \_request -> NoPermissions
      , Orb.handleRequest =
          \_request () ->
            Orb.return200 (S.unifyTaggedUnion @"foo" (Foo True))
      }

type TaggedUnionResponses =
  [ Orb.Response200 TaggedUnionResponse
  , Orb.Response500 Orb.InternalServerError
  ]

type TaggedUnionResponse =
  S.TaggedUnion
    [ "foo" @= Foo
    , "bar" @= Bar
    ]

unionResponseSchema :: FC.Fleece schema => schema TaggedUnionResponse
unionResponseSchema =
  FC.taggedUnionNamed "TaggedUnionResponse" "type" $
    FC.taggedUnionMember @"foo" fooObjectSchema
      #@ FC.taggedUnionMember @"bar" barObjectSchema

data Foo = Foo
  { fooField :: Bool
  }

fooObjectSchema :: FC.Fleece schema => FC.Object schema Foo Foo
fooObjectSchema =
  FC.constructor Foo
    #+ FC.required "fooField" fooField FC.boolean

data Bar = Bar
  { barField :: T.Text
  }

barObjectSchema :: FC.Fleece schema => FC.Object schema Bar Bar
barObjectSchema =
  FC.constructor Bar
    #+ FC.required "barField" barField FC.text
