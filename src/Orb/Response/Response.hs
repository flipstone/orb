{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Orb.Response.Response
  ( respondWith
  , ResponseBodies (..)
  , responseBodyList
  , ResponseBody (..)
  , ResponseContent (..)
  , ResponseData (..)
  , ResponseBodiesBuilder (..)
  , responseBodies
  , noResponseBodies
  )
where

import Control.Monad.IO.Class qualified as MIO
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Map.Strict qualified as Map
import Fleece.Core qualified as FC
import GHC.TypeLits (KnownNat)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Response, ResponseReceived)
import Network.Wai qualified as Wai
import Shrubbery qualified as S

import Orb.HasRespond qualified as HasRespond
import Orb.Response.ContentType (ContentType)

data ResponseBodies (tags :: [S.Tag]) = ResponseBodies
  { encodeResponseBranches :: S.TaggedBranches tags ResponseData
  , responseStatusMap :: Map.Map HTTP.Status ResponseBody
  }

responseBodyList :: ResponseBodies tags -> [(HTTP.Status, ResponseBody)]
responseBodyList =
  Map.toList . responseStatusMap

data ResponseBody where
  NoSchemaResponseBody ::
    Maybe ContentType -> ResponseBody
  SchemaResponseBody ::
    (forall t. FC.Fleece t => FC.Schema t body) -> ResponseBody
  EmptyResponseBody ::
    ResponseBody

data ResponseContent
  = ResponseContentFile FilePath (Maybe Wai.FilePart)
  | ResponseContentBuilder BSB.Builder
  | ResponseContentStream Wai.StreamingBody

data ResponseData = ResponseData
  { responseDataStatus :: HTTP.Status
  , responseDataContent :: ResponseContent
  , responseDataContentType :: Maybe BS.ByteString
  , responseDataExtraHeaders :: HTTP.ResponseHeaders
  }

data ResponseBodiesBuilder (tags :: [S.Tag]) = ResponseBodiesBuilder
  { encodeResponseBranchesBuilder :: S.TaggedBranchBuilder tags ResponseData
  , responseStatusMapBuilder :: Map.Map HTTP.Status ResponseBody
  }

noResponseBodies :: ResponseBodiesBuilder '[]
noResponseBodies =
  ResponseBodiesBuilder
    { encodeResponseBranchesBuilder = S.taggedBranchEnd
    , responseStatusMapBuilder = Map.empty
    }

responseBodies ::
  (KnownNat providedLength, providedLength ~ S.Length (S.TaggedTypes tags)) =>
  ResponseBodiesBuilder tags ->
  ResponseBodies tags
responseBodies builder =
  ResponseBodies
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
