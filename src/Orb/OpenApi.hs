{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Orb.OpenApi
  ( mkOpenApi
  , mkAllOpenApis
  , openApiLabels
  , OpenApiProvider (provideOpenApi)
  , OpenApiRouter
  , FleeceOpenApi
  , SchemaWithComponents (..)
  , schemaWithComponents
  ) where

import Beeline.Routing qualified as R
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.Align qualified as Align
import Data.ByteString.Char8 qualified as BS8
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.These qualified as These
import Fleece.Core qualified as FC
import GHC.TypeLits (symbolVal)
import Network.HTTP.Media.MediaType qualified as MediaType
import Network.HTTP.Types qualified as HTTPTypes

import Orb.Handler qualified as Handler
import Orb.Response qualified as Response

{- |
  This class represents routers that can be used to provide OpenApi
  descriptions of parts of their routes.
-}
class Handler.ServerRouter r => OpenApiProvider r where
  -- |
  --     Labels the provided router with a user-defined string that can be
  --     passed later to 'mkOpenApi' to retrieve the OpenApi description for
  --     that part of the api. If multiple sections are labeled with the
  --     same label, their definitions will be combined to provide a single
  --     OpenApi description.
  provideOpenApi :: String -> r a -> r a

instance OpenApiProvider R.RouteRecognizer where
  provideOpenApi _label = id

{- |
  Retrieves the list of all labels that were passed to 'provideOpenApi'
  within the given router.
-}
openApiLabels :: OpenApiRouter a -> [String]
openApiLabels (OpenApiRouter builders) =
  Map.keys (labeledBuilders builders)

{- |
  Returns all the 'OpenApi.OpenApi' descriptions that have been labeled in
  the specified router. If any of them return an error, it will be returned.
-}
mkAllOpenApis :: OpenApiRouter a -> Either String (Map.Map String OpenApi.OpenApi)
mkAllOpenApis router =
  let
    mkLabeledApi label = do
      api <- mkOpenApi router label
      pure (label, api)
  in
    Map.fromList <$> traverse mkLabeledApi (openApiLabels router)

{- |
  Returns the 'OpenApi.OpenApi' descriptions for the api section that was
  labeled with the provide label via the 'provideOpenApi' function. If
  no section with the label can be found or if an error occurs while generating
  the OpenApi description, an error will be returned.
-}
mkOpenApi :: OpenApiRouter a -> String -> Either String OpenApi.OpenApi
mkOpenApi (OpenApiRouter builders) label = do
  builder <-
    case Map.lookup label (labeledBuilders builders) of
      Nothing -> Left ("No OpenApi definition found with label " <> label <> ".")
      Just builder -> Right builder

  apiInfo <-
    runOpenApiBuilder builder $
      PathInfo
        { pathInfoPath = ""
        , pathInfoParams = []
        }

  let
    paths =
      toIOHM . apiPaths $ apiInfo

    componentsSchemas =
      toIOHM
        . fmap openApiSchema
        . apiSchemaComponents
        $ apiInfo

  pure $
    mempty
      { OpenApi._openApiPaths = paths
      , OpenApi._openApiComponents =
          mempty
            { OpenApi._componentsSchemas = componentsSchemas
            }
      }

toIOHM :: Hashable k => Map.Map k v -> IOHM.InsOrdHashMap k v
toIOHM =
  IOHM.fromList . Map.toList

data ApiInfo = ApiInfo
  { apiPaths :: Map.Map FilePath OpenApi.PathItem
  , apiSchemaComponents :: Map.Map T.Text SchemaInfo
  }

emptyApiInfo :: ApiInfo
emptyApiInfo =
  ApiInfo
    { apiPaths = Map.empty
    , apiSchemaComponents = Map.empty
    }

combineApiInfo :: ApiInfo -> ApiInfo -> Either String ApiInfo
combineApiInfo left right = do
  components <-
    combineSchemaComponents
      (apiSchemaComponents left)
      (apiSchemaComponents right)
  pure $
    ApiInfo
      { apiPaths =
          Map.unionWith combinePathItems (apiPaths left) (apiPaths right)
      , apiSchemaComponents = components
      }

{- |
  The Semigroup instance of 'OpenApi.PathItem' concatenates all the parameters
  from both path items. Since by definition these items were at the same path,
  they should all have generated the same parameters from the Beeline route so
  we arbitrary pick just the left params instead.
-}
combinePathItems :: OpenApi.PathItem -> OpenApi.PathItem -> OpenApi.PathItem
combinePathItems left right =
  (left <> right)
    { OpenApi._pathItemParameters = OpenApi._pathItemParameters left
    }

combineSchemaComponents ::
  Map.Map T.Text SchemaInfo ->
  Map.Map T.Text SchemaInfo ->
  Either String (Map.Map T.Text SchemaInfo)
combineSchemaComponents left right =
  let
    checkForConflict theseSchemas =
      case theseSchemas of
        These.This this -> Right this
        These.That that -> Right that
        These.These this that ->
          if this == that
            then Right this
            else
              Left $
                "Conflicting schema definitions found "
                  <> FC.nameToString (fleeceName this)
                  <> " and "
                  <> FC.nameToString (fleeceName that)

    addKeyToError :: T.Text -> Either String a -> Either String a
    addKeyToError key errOrSchemaInfo =
      case errOrSchemaInfo of
        Left err -> Left (T.unpack key <> ": " <> err)
        Right schemaInfo -> Right schemaInfo
  in
    Map.traverseWithKey
      addKeyToError
      (Align.alignWith checkForConflict left right)

singletonApiInfo ::
  Map.Map T.Text SchemaInfo ->
  PathInfo ->
  OpenApi.PathItem ->
  ApiInfo
singletonApiInfo components pathInfo pathItem =
  ApiInfo
    { apiPaths = Map.singleton (pathInfoPath pathInfo) pathItem
    , apiSchemaComponents = components
    }

newtype OpenApiBuilder
  = OpenApiBuilder (PathInfo -> Either String ApiInfo)

runOpenApiBuilder :: OpenApiBuilder -> PathInfo -> Either String ApiInfo
runOpenApiBuilder (OpenApiBuilder f) =
  f

modifyOpenApiBuilderPathInfo ::
  (PathInfo -> PathInfo) ->
  OpenApiBuilder ->
  OpenApiBuilder
modifyOpenApiBuilderPathInfo f (OpenApiBuilder mkApiInfo) =
  OpenApiBuilder $ \pathInfo -> mkApiInfo (f pathInfo)

emptyOpenApiBuilder :: OpenApiBuilder
emptyOpenApiBuilder =
  OpenApiBuilder (const (Right emptyApiInfo))

combineOpenApiBuilder ::
  OpenApiBuilder ->
  OpenApiBuilder ->
  OpenApiBuilder
combineOpenApiBuilder (OpenApiBuilder left) (OpenApiBuilder right) =
  OpenApiBuilder $ \parentContext ->
    Monad.join $
      liftA2
        combineApiInfo
        (left parentContext)
        (right parentContext)

data OpenApiBuilders = OpenApiBuilders
  { labeledBuilders :: Map.Map String OpenApiBuilder
  , unlabeledBuilder :: OpenApiBuilder
  }

emptyOpenApiBuilders :: OpenApiBuilders
emptyOpenApiBuilders =
  OpenApiBuilders
    { labeledBuilders = Map.empty
    , unlabeledBuilder = emptyOpenApiBuilder
    }

modifyOpenApiBuildersPathInfo ::
  (PathInfo -> PathInfo) ->
  OpenApiBuilders ->
  OpenApiBuilders
modifyOpenApiBuildersPathInfo f builders =
  OpenApiBuilders
    { labeledBuilders = modifyOpenApiBuilderPathInfo f <$> labeledBuilders builders
    , unlabeledBuilder = modifyOpenApiBuilderPathInfo f (unlabeledBuilder builders)
    }

combineOpenApiBuilders ::
  OpenApiBuilders ->
  OpenApiBuilders ->
  OpenApiBuilders
combineOpenApiBuilders left right =
  OpenApiBuilders
    { labeledBuilders =
        Map.unionWith
          combineOpenApiBuilder
          (labeledBuilders left)
          (labeledBuilders right)
    , unlabeledBuilder =
        combineOpenApiBuilder
          (unlabeledBuilder left)
          (unlabeledBuilder right)
    }

{- |
  A concrete type that implements the beeline and Orb router classes and
  can be used with 'mkOpenApi'.
-}
newtype OpenApiRouter a
  = OpenApiRouter OpenApiBuilders

instance OpenApiProvider OpenApiRouter where
  provideOpenApi name (OpenApiRouter builders) =
    let
      additionalBuilders =
        emptyOpenApiBuilders
          { labeledBuilders =
              Map.singleton name (unlabeledBuilder builders)
          }
    in
      OpenApiRouter $
        combineOpenApiBuilders additionalBuilders builders

instance Handler.ServerRouter OpenApiRouter where
  methodHandler method handler (Builder mkRoute) =
    let
      builder =
        OpenApiBuilder $ \parentContext -> do
          mbRequestBody <- mkRequestBody handler

          let
            (mbReqBody, reqComponents) =
              case mbRequestBody of
                Nothing -> (Nothing, Map.empty)
                Just (reqBody, reqComps) -> (Just reqBody, reqComps)

          (responses, responseComponents) <- mkResponses handler
          allComponents <- combineSchemaComponents reqComponents responseComponents

          let
            operation =
              mempty
                { OpenApi._operationOperationId = Just . T.pack . Handler.handlerId $ handler
                , OpenApi._operationRequestBody = mbReqBody
                , OpenApi._operationResponses = responses
                }

            pathInfo =
              mkRoute parentContext

            mbPathItem =
              mkPathItem method pathInfo operation

          case mbPathItem of
            Nothing ->
              Left $
                "Unable to create OpenAPI for description of "
                  <> pathInfoPath pathInfo
                  <> " due to use of HTTP Method: "
                  <> BS8.unpack (HTTPTypes.renderStdMethod method)
            Just pathItem -> do
              pure $ singletonApiInfo allComponents pathInfo pathItem
    in
      OpenApiRouter $
        emptyOpenApiBuilders
          { unlabeledBuilder = builder
          }

data PathInfo = PathInfo
  { pathInfoPath :: FilePath
  , pathInfoParams :: [OpenApi.Param]
  }

addPathPiece :: PathInfo -> String -> PathInfo
addPathPiece pathInfo piece =
  pathInfo
    { pathInfoPath = pathInfoPath pathInfo <> "/" <> piece
    }

addPathParam :: PathInfo -> OpenApi.Param -> PathInfo
addPathParam pathInfo param =
  pathInfo
    { pathInfoPath =
        pathInfoPath pathInfo
          <> "/{"
          <> T.unpack (OpenApi._paramName param)
          <> "}"
    , pathInfoParams =
        pathInfoParams pathInfo <> [param]
    }

instance R.Router OpenApiRouter where
  newtype RouteList OpenApiRouter _subRoutes
    = RouteList OpenApiBuilders

  newtype Builder OpenApiRouter _router _a
    = Builder (PathInfo -> PathInfo)

  make _route =
    Builder id

  piece (Builder mkRoute) pathPiece =
    Builder $ \parentContext ->
      addPathPiece (mkRoute parentContext) (T.unpack pathPiece)

  param (Builder mkRoute) param =
    Builder $ \parentContext ->
      addPathParam
        (mkRoute parentContext)
        (mkOpenApiPathParam (R.paramDefinition param))

  method method (Builder mkRoute) =
    let
      builder =
        OpenApiBuilder $
          \parentContext ->
            let
              pathInfo =
                mkRoute parentContext
            in
              Left
                ( "Unable to make OpenAPI description for router defined using standard Beeline 'method' (or helpers such as 'get'): "
                    <> BS8.unpack (HTTPTypes.renderStdMethod method)
                    <> " "
                    <> pathInfoPath pathInfo
                )
    in
      OpenApiRouter $
        emptyOpenApiBuilders
          { unlabeledBuilder = builder
          }

  subrouter (Builder mkRoute) subrouter =
    OpenApiRouter $
      let
        (OpenApiRouter openApiBuilders) =
          R.subrouteRouter subrouter
      in
        modifyOpenApiBuildersPathInfo mkRoute openApiBuilders

  routeList (RouteList mkRoutes) =
    OpenApiRouter mkRoutes

  addRoute (OpenApiRouter routerBuilders) (RouteList listBuilders) =
    RouteList (combineOpenApiBuilders routerBuilders listBuilders)

  emptyRoutes =
    RouteList emptyOpenApiBuilders

mkPathItem ::
  HTTPTypes.StdMethod ->
  PathInfo ->
  OpenApi.Operation ->
  Maybe OpenApi.PathItem
mkPathItem method pathInfo operation =
  let
    baseItem =
      mempty
        { OpenApi._pathItemParameters =
            fmap OpenApi.Inline (pathInfoParams pathInfo)
        }
  in
    case method of
      HTTPTypes.GET -> Just $ baseItem {OpenApi._pathItemGet = Just operation}
      HTTPTypes.POST -> Just $ baseItem {OpenApi._pathItemPost = Just operation}
      HTTPTypes.PUT -> Just $ baseItem {OpenApi._pathItemPut = Just operation}
      HTTPTypes.HEAD -> Just $ baseItem {OpenApi._pathItemHead = Just operation}
      HTTPTypes.DELETE -> Just $ baseItem {OpenApi._pathItemDelete = Just operation}
      HTTPTypes.TRACE -> Just $ baseItem {OpenApi._pathItemTrace = Just operation}
      HTTPTypes.OPTIONS -> Just $ baseItem {OpenApi._pathItemOptions = Just operation}
      HTTPTypes.PATCH -> Just $ baseItem {OpenApi._pathItemPatch = Just operation}
      HTTPTypes.CONNECT -> Nothing

mkRequestBody ::
  Handler.Handler route ->
  Either String (Maybe (OpenApi.Referenced OpenApi.RequestBody, Map.Map T.Text SchemaInfo))
mkRequestBody handler =
  case Handler.requestBody handler of
    Handler.RequestSchema (FleeceOpenApi errOrSchemaInfo) -> do
      schemaInfo <- fmap rewriteSchemaInfo errOrSchemaInfo

      let
        schemaRef =
          mkSchemaRef schemaInfo

        mediaTypeObject =
          mempty
            { OpenApi._mediaTypeObjectSchema = Just schemaRef
            }

        requestBody =
          OpenApi.Inline $
            OpenApi.RequestBody
              { OpenApi._requestBodyDescription = Nothing
              , OpenApi._requestBodyRequired = Just True
              , OpenApi._requestBodyContent =
                  IOHM.fromList
                    [ (mkMediaType "application" "json", mediaTypeObject)
                    ]
              }

      components <- collectComponents [schemaInfo]
      pure $ Just (requestBody, components)
    Handler.RequestRawBody _decoder ->
      Right Nothing
    Handler.RequestFormData _formDecoder ->
      Right Nothing
    Handler.EmptyRequestBody ->
      Right Nothing

mkResponses ::
  Handler.Handler router ->
  Either String (OpenApi.Responses, Map.Map T.Text SchemaInfo)
mkResponses handler =
  let
    schemas =
      Response.responseBodyList . Handler.handlerResponseBodies $ handler

    addResponse (responses, components) (status, responseSchema) = do
      mbSchemaInfo <-
        case responseSchema of
          Response.ResponseContent _contentType _encoder -> pure Nothing
          Response.ResponseSchema (FleeceOpenApi info) -> fmap Just info
          Response.ResponseDocument -> pure Nothing
          Response.EmptyResponseBody -> pure Nothing
      let
        mkResponseContent schemaRef =
          IOHM.fromList
            [ (mkMediaType "application" "json", mempty {OpenApi._mediaTypeObjectSchema = Just schemaRef})
            ]

        mbResponseContent =
          fmap (mkResponseContent . mkSchemaRef) mbSchemaInfo

        response =
          OpenApi.Inline $
            OpenApi.Response
              { OpenApi._responseDescription = T.empty
              , OpenApi._responseContent = Maybe.fromMaybe mempty mbResponseContent
              , OpenApi._responseHeaders = IOHM.empty
              , OpenApi._responseLinks = IOHM.empty
              }

        newResponses =
          responses
            { OpenApi._responsesResponses =
                IOHM.insert
                  (HTTPTypes.statusCode status)
                  response
                  (OpenApi._responsesResponses responses)
            }

      newComponents <-
        combineSchemaComponents components =<< collectComponents (Maybe.maybeToList mbSchemaInfo)

      pure (newResponses, newComponents)
  in
    Monad.foldM addResponse (mempty, Map.empty) schemas

mkMediaType :: String -> String -> MediaType.MediaType
mkMediaType main sub =
  (MediaType.//) (BS8.pack main) (BS8.pack sub)

mkProperties :: [FieldInfo] -> IOHM.InsOrdHashMap T.Text (OpenApi.Referenced OpenApi.Schema)
mkProperties =
  let
    mkEntry fieldInfo =
      (fieldName fieldInfo, mkSchemaRef (fieldSchemaInfo fieldInfo))
  in
    IOHM.fromList . fmap mkEntry

mkRequiredProperties :: [FieldInfo] -> [OpenApi.ParamName]
mkRequiredProperties =
  fmap fieldName
    . List.filter fieldRequired

mkOpenApiPathParam :: R.ParameterDefinition a -> OpenApi.Param
mkOpenApiPathParam param =
  mempty
    { OpenApi._paramName = R.parameterName param
    , OpenApi._paramIn = OpenApi.ParamPath
    , OpenApi._paramRequired = Just True
    , OpenApi._paramSchema =
        Just
          . OpenApi.Inline
          $ mempty
            { OpenApi._schemaType = Just OpenApi.OpenApiString
            }
    }

{- |
  A concrete type that implements the Fleece typeclasses to build an
  OpenApi description of the JSON describe by the Fleece schema definition.
  Can be used with 'schemaWithComponents' to retrieve the 'OpenApi.Schema'
  description of the schema.
-}
newtype FleeceOpenApi a = FleeceOpenApi
  { unFleeceOpenApi :: Either String SchemaInfo
  }

{- |
  An 'OpenApi.Schema' pair with any other component schemas that it is
  dependent on. This can be construction from a Fleece JSON schema via the
  'schemaWithComponents' function.
-}
data SchemaWithComponents = SchemaWithComponents
  { schemaWithComponentsSchema :: OpenApi.Schema
  , schemaWithComponentsComponents :: IOHM.InsOrdHashMap T.Text OpenApi.Schema
  }

{- |
  Constructions a 'SchemaWithComponents' OpenApi description of the provided
  Fleece schema. If an error occurs during construction (e.g. conflicting
  definitions of the same component), an error will be returned.
-}
schemaWithComponents :: FleeceOpenApi a -> Either String SchemaWithComponents
schemaWithComponents =
  fmap
    ( \schemaInfo ->
        SchemaWithComponents
          { schemaWithComponentsSchema = openApiSchema schemaInfo
          , schemaWithComponentsComponents =
              Map.foldMapWithKey
                (\key -> IOHM.singleton key . openApiSchema)
                (schemaComponents schemaInfo)
          }
    )
    . unFleeceOpenApi

data SchemaInfo = SchemaInfo
  { fleeceName :: FC.Name
  , schemaIsPrimitive :: Bool
  , openApiKey :: Maybe T.Text
  , openApiSchema :: OpenApi.Schema
  , schemaComponents :: Map.Map T.Text SchemaInfo
  }
  deriving (Eq)

setSchemaInfoFormat :: T.Text -> SchemaInfo -> SchemaInfo
setSchemaInfoFormat fmt info =
  info
    { openApiSchema =
        (openApiSchema info)
          { OpenApi._schemaFormat = Just fmt
          }
    }

collectComponents :: [SchemaInfo] -> Either String (Map.Map T.Text SchemaInfo)
collectComponents schemaInfos =
  let
    mkTopLevel schemaInfo =
      case openApiKey schemaInfo of
        Nothing -> Map.empty
        Just key -> Map.singleton key schemaInfo

    topLevels =
      fmap mkTopLevel schemaInfos

    descendants =
      fmap schemaComponents schemaInfos
  in
    Monad.foldM
      combineSchemaComponents
      Map.empty
      (topLevels <> descendants)

mkSchemaRef ::
  SchemaInfo ->
  OpenApi.Referenced OpenApi.Schema
mkSchemaRef schema =
  case openApiKey schema of
    Just schemaKey ->
      OpenApi.Ref . OpenApi.Reference $ schemaKey
    Nothing ->
      OpenApi.Inline . openApiSchema $ schema

mkPrimitiveSchema ::
  String ->
  OpenApi.OpenApiType ->
  SchemaInfo
mkPrimitiveSchema name openApiType =
  SchemaInfo
    { fleeceName = FC.unqualifiedName name
    , schemaIsPrimitive = True
    , openApiKey = Nothing
    , openApiSchema =
        mempty
          { OpenApi._schemaType = Just openApiType
          }
    , schemaComponents = Map.empty
    }

data FieldInfo = FieldInfo
  { fieldName :: T.Text
  , fieldRequired :: Bool
  , fieldSchemaInfo :: SchemaInfo
  }

instance FC.Fleece FleeceOpenApi where
  data Object FleeceOpenApi _object _constructor
    = Object (Either String [FieldInfo])

  data Field FleeceOpenApi _object _field
    = Field (Either String FieldInfo)

  data AdditionalFields FleeceOpenApi _object _field
    = AdditionalFields

  data UnionMembers FleeceOpenApi _allTypes _handledTypes
    = UnionMembers (Either String [SchemaInfo])

  data TaggedUnionMembers FleeceOpenApi _allTags _handledTags
    = TaggedUnionMembers (FieldInfo -> String -> Either String [(T.Text, SchemaInfo)])

  schemaName (FleeceOpenApi errOrSchemaInfo) =
    case errOrSchemaInfo of
      Left err -> FC.unqualifiedName ("Unable to get schema name:" <> err)
      Right schemaInfo -> fleeceName schemaInfo

  number =
    FleeceOpenApi . Right $ mkPrimitiveSchema "number" OpenApi.OpenApiNumber

  text =
    FleeceOpenApi . Right $ mkPrimitiveSchema "text" OpenApi.OpenApiString

  boolean =
    FleeceOpenApi . Right $ mkPrimitiveSchema "boolean" OpenApi.OpenApiBoolean

  null =
    FleeceOpenApi . Right $ mkPrimitiveSchema "null" OpenApi.OpenApiNull

  array (FleeceOpenApi errOrItemSchemaInfo) =
    FleeceOpenApi $ do
      itemSchemaInfo <- errOrItemSchemaInfo
      components <- collectComponents [itemSchemaInfo]

      let
        itemSchemaRef =
          mkSchemaRef itemSchemaInfo

      pure $
        SchemaInfo
          { fleeceName = FC.annotateName (fleeceName itemSchemaInfo) "array"
          , schemaIsPrimitive = False
          , openApiKey = Nothing
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Just OpenApi.OpenApiArray
                , OpenApi._schemaItems = Just (OpenApi.OpenApiItemsObject itemSchemaRef)
                }
          , schemaComponents = components
          }

  nullable (FleeceOpenApi errOrSchemaInfo) =
    FleeceOpenApi $ do
      schemaInfo <- fmap rewriteSchemaInfo errOrSchemaInfo
      components <- collectComponents [schemaInfo]

      pure $
        SchemaInfo
          { fleeceName = FC.annotateName (fleeceName schemaInfo) "nullable"
          , schemaIsPrimitive = schemaIsPrimitive schemaInfo
          , openApiKey = Nothing
          , openApiSchema =
              (openApiSchema schemaInfo)
                { OpenApi._schemaNullable = Just True
                }
          , schemaComponents = components
          }

  required name _accessor (FleeceOpenApi errOrSchemaInfo) =
    Field $ do
      schemaInfo <- fmap rewriteSchemaInfo errOrSchemaInfo
      pure $
        FieldInfo
          { fieldName = T.pack name
          , fieldRequired = True
          , fieldSchemaInfo = schemaInfo
          }

  optional name _accessor (FleeceOpenApi errOrSchemaInfo) =
    Field $ do
      schemaInfo <- fmap rewriteSchemaInfo errOrSchemaInfo
      pure $
        FieldInfo
          { fieldName = T.pack name
          , fieldRequired = False
          , fieldSchemaInfo = schemaInfo
          }

  mapField _f (Field fieldInfo) =
    Field fieldInfo

  additionalFields _accessor _schema =
    AdditionalFields

  objectNamed name (Object errOrFieldsInReverse) =
    FleeceOpenApi (mkObjectForFields name =<< errOrFieldsInReverse)

  constructor _cons =
    Object (Right [])

  field (Object fieldInfos) (Field newFieldInfo) =
    Object (liftA2 (:) newFieldInfo fieldInfos)

  additional (Object _fields) _additional =
    Object (Left "Fleece additional fields not currently support for OpenAPI")

  validateNamed name _uncheck _check (FleeceOpenApi errOrSchemaInfo) = do
    FleeceOpenApi $ do
      schemaInfo <- fmap rewriteSchemaInfo errOrSchemaInfo

      if schemaIsPrimitive schemaInfo
        then do
          components <- collectComponents [schemaInfo]
          pure $
            schemaInfo
              { fleeceName = name
              , openApiKey = Just . fleeceNameToOpenApiKey $ name
              , schemaComponents = components
              }
        else pure schemaInfo

  boundedEnumNamed name toText =
    let
      enumValues =
        fmap
          (Aeson.toJSON . toText)
          [minBound .. maxBound]
    in
      FleeceOpenApi
        . Right
        $ SchemaInfo
          { fleeceName = name
          , schemaIsPrimitive = False
          , openApiKey = Just . fleeceNameToOpenApiKey $ name
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Just OpenApi.OpenApiString
                , OpenApi._schemaEnum = Just enumValues
                }
          , schemaComponents = Map.empty
          }

  unionNamed name (UnionMembers errOrMembers) =
    FleeceOpenApi $ do
      let
        key = Just $ fleeceNameToOpenApiKey name

      members <- errOrMembers
      components <- collectComponents members

      pure $
        SchemaInfo
          { fleeceName = name
          , schemaIsPrimitive = False
          , openApiKey = key
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Nothing
                , OpenApi._schemaDiscriminator = Nothing
                , OpenApi._schemaOneOf =
                    Just $ fmap mkSchemaRef members
                , OpenApi._schemaTitle = key
                }
          , schemaComponents = components
          }

  unionMemberWithIndex _idx (FleeceOpenApi errOrSchemaInfo) =
    UnionMembers $ do
      schemaInfo <- errOrSchemaInfo
      pure [rewriteSchemaInfo schemaInfo]

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers $ liftA2 (<>) left right

  taggedUnionNamed name tagPropertyString (TaggedUnionMembers mkMembers) =
    FleeceOpenApi $ do
      let
        tagProperty =
          T.pack tagPropertyString

        memberKeyPrefix =
          FC.nameUnqualified name <> "."

        FleeceOpenApi errOrStringSchema =
          FC.text

        mkTagField tagSchema =
          FieldInfo
            { fieldName = tagProperty
            , fieldRequired = True
            , fieldSchemaInfo = tagSchema
            }

        mkMappingEntry (tagValue, schemaInfo) =
          case openApiKey schemaInfo of
            Just key -> Right (tagValue, componentsPrefix <> key)
            Nothing ->
              Left $
                "No Schema Key found for member "
                  <> T.unpack tagValue
                  <> " of union "
                  <> FC.nameToString name

      stringSchema <- errOrStringSchema
      members <- mkMembers (mkTagField stringSchema) memberKeyPrefix

      components <- collectComponents (fmap snd members)

      mappingEntries <- traverse mkMappingEntry members

      let
        discriminator =
          OpenApi.Discriminator
            { OpenApi._discriminatorPropertyName = tagProperty
            , OpenApi._discriminatorMapping = IOHM.fromList mappingEntries
            }

        memberSchemaRefs =
          fmap (mkSchemaRef . snd) members

        key =
          Just $ fleeceNameToOpenApiKey name

      pure $
        SchemaInfo
          { fleeceName = name
          , schemaIsPrimitive = False
          , openApiKey = key
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Nothing
                , OpenApi._schemaDiscriminator = Just discriminator
                , OpenApi._schemaOneOf = Just memberSchemaRefs
                , OpenApi._schemaTitle = key
                }
          , schemaComponents = components
          }

  taggedUnionMemberWithTag tag (Object errOrFieldsInReverse) =
    TaggedUnionMembers $ \tagField memberKeyPrefix -> do
      let
        tagValue =
          symbolVal tag

        -- TODO: come up with better name
        memberName =
          FC.unqualifiedName (memberKeyPrefix <> tagValue)

      -- TODO: add tag property to schema
      fieldsInReverse <- errOrFieldsInReverse

      let
        fieldsInReverseWithTag =
          fieldsInReverse <> [tagField]

      objectSchema <- mkObjectForFields memberName fieldsInReverseWithTag

      pure [(T.pack tagValue, objectSchema)]

  taggedUnionCombine (TaggedUnionMembers mkLeft) (TaggedUnionMembers mkRight) =
    TaggedUnionMembers $ \tagProperty memberKeyPrefix -> do
      left <- mkLeft tagProperty memberKeyPrefix
      right <- mkRight tagProperty memberKeyPrefix
      pure (left <> right)

  jsonString (FleeceOpenApi _schemaInfo) =
    FleeceOpenApi
      . Left
      $ "Fleece jsonString is not currently implemented for OpenApi"

mkObjectForFields ::
  FC.Name ->
  [FieldInfo] ->
  Either String SchemaInfo
mkObjectForFields name fieldsInReverse = do
  let
    key =
      Just $ fleeceNameToOpenApiKey name
    fieldsInOrder =
      List.reverse fieldsInReverse

  components <- collectComponents (fmap fieldSchemaInfo fieldsInOrder)

  let

  pure $
    SchemaInfo
      { fleeceName = name
      , schemaIsPrimitive = False
      , openApiKey = key
      , openApiSchema =
          mempty
            { OpenApi._schemaType = Just OpenApi.OpenApiObject
            , OpenApi._schemaProperties = mkProperties fieldsInOrder
            , OpenApi._schemaRequired = mkRequiredProperties fieldsInOrder
            , OpenApi._schemaTitle = key
            }
      , schemaComponents = components
      }

fleeceNameToOpenApiKey :: FC.Name -> T.Text
fleeceNameToOpenApiKey =
  T.pack . FC.nameUnqualified

-- TODO this is a hack pending a better solution in json-fleece.
rewriteSchemaInfo :: SchemaInfo -> SchemaInfo
rewriteSchemaInfo schemaInfo =
  case FC.nameUnqualified $ fleeceName schemaInfo of
    "Int" -> mkPrimitiveSchema "integer" OpenApi.OpenApiInteger
    "Int64" -> setSchemaInfoFormat "int64" $ mkPrimitiveSchema "integer" OpenApi.OpenApiInteger
    "Int32" -> setSchemaInfoFormat "int32" $ mkPrimitiveSchema "integer" OpenApi.OpenApiInteger
    "UTCTime" -> setSchemaInfoFormat "date-time" $ mkPrimitiveSchema "string" OpenApi.OpenApiString
    _ -> schemaInfo

componentsPrefix :: T.Text
componentsPrefix =
  "#/components/schemas/"
