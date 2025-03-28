{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Orb.Response.Response
  ( respondWith
  , ResponseSchemas (..)
  , responseSchemaList
  , ResponseSchema (..)
  , ResponseData (..)
  , ResponseSchemasBuilder (..)
  , responseSchemas
  , noResponseSchemas
  )
where

import Control.Monad.IO.Class qualified as MIO
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Fleece.Core qualified as FC
import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Response, ResponseReceived)
import Shrubbery qualified as S

import Orb.HasRespond qualified as HasRespond

data ResponseSchemas (tags :: [S.Tag]) = ResponseSchemas
  { encodeResponseBranches :: S.TaggedBranches tags ResponseData
  , responseStatusMap :: Map.Map HTTP.Status ResponseSchema
  }

responseSchemaList :: ResponseSchemas tags -> [(HTTP.Status, ResponseSchema)]
responseSchemaList =
  Map.toList . responseStatusMap

data ResponseSchema where
  ResponseSchema :: (forall schema. FC.Fleece schema => schema a) -> ResponseSchema
  NoResponseSchema :: ResponseSchema
  ResponseDocument :: ResponseSchema

data ResponseData = ResponseData
  { responseDataStatus :: HTTP.Status
  , responseDataBytes :: LBS.ByteString
  , responseDataContentType :: Maybe BS.ByteString
  , responseDataExtraHeaders :: HTTP.ResponseHeaders
  }

data ResponseSchemasBuilder (tags :: [S.Tag]) = ResponseSchemasBuilder
  { encodeResponseBranchesBuilder :: S.TaggedBranchBuilder tags ResponseData
  , responseStatusMapBuilder :: Map.Map HTTP.Status ResponseSchema
  }

noResponseSchemas :: ResponseSchemasBuilder '[]
noResponseSchemas =
  ResponseSchemasBuilder
    { encodeResponseBranchesBuilder = S.taggedBranchEnd
    , responseStatusMapBuilder = Map.empty
    }

responseSchemas ::
  (KnownNat length, length ~ S.Length (S.TaggedTypes tags)) =>
  ResponseSchemasBuilder tags ->
  ResponseSchemas tags
responseSchemas builder =
  ResponseSchemas
    { encodeResponseBranches = S.taggedBranchBuild (encodeResponseBranchesBuilder builder)
    , responseStatusMap = responseStatusMapBuilder builder
    }

respondWith ::
  (MIO.MonadIO m, HasRespond.HasRespond m) =>
  Response ->
  m ResponseReceived
respondWith r = do
  respond <- HasRespond.respond
  MIO.liftIO $ respond r
