{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Orb.Response.AddResponseHeaders
  ( addResponseHeaders
  , AddResponseHeaderBranches
  ) where

import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTP
import Shrubbery (type (@=))
import Shrubbery qualified as S

{- |
Adds extra response headers to a response, typically one produced by a handler
to be returned for Orb to encode as the response body. This function adds the
headers without respect to which response body type or status code is tagged on
the response.

@since 0.1.1
-}
addResponseHeaders ::
  ( AddResponseHeaderBranches responseCodes responseCodes
  , KnownNat (S.Length (S.TaggedTypes responseCodes))
  ) =>
  S.TaggedUnion responseCodes ->
  HTTP.ResponseHeaders ->
  S.TaggedUnion responseCodes
addResponseHeaders =
  S.dissectTaggedUnion (S.taggedBranchBuild addResponseHeaderBranchBuilder)

class AddResponseHeaderBranches allResponseCodes someResponseCodes where
  addResponseHeaderBranchBuilder ::
    S.TaggedBranchBuilder
      someResponseCodes
      (HTTP.ResponseHeaders -> S.TaggedUnion allResponseCodes)

instance AddResponseHeaderBranches allResponseCodes '[] where
  addResponseHeaderBranchBuilder =
    S.taggedBranchEnd

instance
  ( AddResponseHeaderBranches allResponseCodes someResponseCodes
  , (returnType, HTTP.ResponseHeaders) ~ S.TagType code allResponseCodes
  , index ~ S.TagIndex code allResponseCodes
  , (returnType, HTTP.ResponseHeaders) ~ S.TypeAtIndex index (S.TaggedTypes allResponseCodes)
  , KnownNat index
  ) =>
  AddResponseHeaderBranches
    allResponseCodes
    (code @= (returnType, HTTP.ResponseHeaders) : someResponseCodes)
  where
  addResponseHeaderBranchBuilder =
    S.taggedBranch
      @code
      (\r h -> S.unifyTaggedUnion @code (addHeaders r h))
      addResponseHeaderBranchBuilder

addHeaders ::
  (responseType, HTTP.ResponseHeaders) ->
  HTTP.ResponseHeaders ->
  (responseType, HTTP.ResponseHeaders)
addHeaders (response, existingHeaders) newHeaders =
  -- newHeaders in put on the front here to avoid rebuilding the linked-list spine
  -- of existingHeaders, which we generally assume will be longer than newHeaders
  (response, newHeaders <> existingHeaders)
