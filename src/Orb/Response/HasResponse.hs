{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Orb.Response.HasResponse
  ( Has400Response
  , Has401Response
  , Has403Response
  , Has404Response
  , Has409Response
  , Has422Response
  , Has500Response
  , HasResponseCodeWithType
  )
where

import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTPTypes
import Shrubbery qualified as S

import Orb.Response.Schemas qualified as Schemas

type Has400Response tags =
  HasResponseCodeWithType tags "400" Schemas.BadRequestMessage

type Has401Response tags =
  HasResponseCodeWithType tags "401" Schemas.UnauthorizedMessage

type Has403Response tags =
  HasResponseCodeWithType tags "403" Schemas.UnauthorizedMessage

type Has404Response tags =
  HasResponseCodeWithType tags "404" Schemas.NotFoundMessage

type Has409Response tags =
  HasResponseCodeWithType tags "409" Schemas.ConflictMessage

type Has422Response tags =
  HasResponseCodeWithType tags "422" Schemas.UnprocessableContentMessage

type Has500Response tags =
  HasResponseCodeWithType tags "500" Schemas.InternalServerError

type HasResponseCodeWithType tags statusCode responseType =
  ( KnownNat (S.TagIndex statusCode tags)
  , S.TagType statusCode tags ~ (responseType, HTTPTypes.ResponseHeaders)
  , S.TypeAtIndex
      (S.TagIndex statusCode tags)
      (S.TaggedTypes tags)
      ~ (responseType, HTTPTypes.ResponseHeaders)
  )
