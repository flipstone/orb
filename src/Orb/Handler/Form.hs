module Orb.Handler.Form
  ( Form
  , getForm
  , FormField (..)
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl')
import Data.List.NonEmpty qualified as NEL
import Data.Map.Merge.Strict (mergeA, traverseMissing, zipWithAMatched)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Wai.Parse qualified as Wai

type Form = Map.Map T.Text FormField

getForm :: ([Wai.Param], [Wai.File LBS.ByteString]) -> Either T.Text Form
getForm (params, files) =
  mergeA
    (traverseMissing $ \_lk lv -> Right lv)
    (traverseMissing $ \_rk rv -> Right rv)
    ( zipWithAMatched $ \k _lv _rv ->
        Left $
          T.pack "Form field with name "
            <> k
            <> T.pack " appears more than once."
    )
    (ParamField <$> foldl' insertParamField Map.empty params)
    (FileField <$> foldl' insertFileField Map.empty files)

insertParamField ::
  Map.Map T.Text (NEL.NonEmpty T.Text) ->
  (BS.ByteString, BS.ByteString) ->
  Map.Map T.Text (NEL.NonEmpty T.Text)
insertParamField params (k, v) =
  Map.insertWith
    (<>)
    (TE.decodeUtf8 k)
    (NEL.singleton $ TE.decodeUtf8 v)
    params

insertFileField ::
  Map.Map T.Text (NEL.NonEmpty (Wai.FileInfo LBS.ByteString)) ->
  (BS.ByteString, Wai.FileInfo LBS.ByteString) ->
  Map.Map T.Text (NEL.NonEmpty (Wai.FileInfo LBS.ByteString))
insertFileField files (k, v) =
  Map.insertWith
    (<>)
    (TE.decodeUtf8 k)
    (NEL.singleton v)
    files

data FormField
  = ParamField (NEL.NonEmpty T.Text)
  | FileField (NEL.NonEmpty (Wai.FileInfo LBS.ByteString))
