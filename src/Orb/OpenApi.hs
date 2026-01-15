{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Orb.OpenApi
  ( mkOpenApi
  , mkAllOpenApis
  , OpenApiError
  , renderOpenApiError
  , openApiLabels
  , OpenApiOptions (openApiAllowedSchemaNameChars)
  , defaultOpenApiOptions
  , OpenApiProvider (provideOpenApi)
  , OpenApiRouter
  , FleeceOpenApi
  , SchemaWithComponents (..)
  , schemaWithComponents
  ) where

import Beeline.Params qualified as BP
import Beeline.Routing qualified as R
import Control.Monad qualified as Monad
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans qualified as Trans
import Data.Aeson qualified as Aeson
import Data.Align qualified as Align
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Char8 qualified as BS8
import Data.DList qualified as DList
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.OpenApi qualified as OpenApi
import Data.Semialign.Indexed qualified as IAlign
import Data.Set qualified as Set
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
  {- |
    Labels the provided router with a user-defined string that can be
    passed later to 'mkOpenApi' to retrieve the OpenApi description for
    that part of the api. If multiple sections are labeled with the
    same label, their definitions will be combined to provide a single
    OpenApi description.
  -}
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
mkAllOpenApis :: OpenApiOptions -> OpenApiRouter a -> Either [OpenApiError] (Map.Map String OpenApi.OpenApi)
mkAllOpenApis options router =
  let
    mkLabeledApi label = do
      api <- mkOpenApi options router label
      pure (label, api)
  in
    Map.fromList <$> traverse mkLabeledApi (openApiLabels router)

{- |
  Returns the 'OpenApi.OpenApi' descriptions for the api section that was
  labeled with the provide label via the 'provideOpenApi' function. If
  no section with the label can be found or if an error occurs while generating
  the OpenApi description, an error will be returned.
-}
mkOpenApi :: OpenApiOptions -> OpenApiRouter a -> String -> Either [OpenApiError] OpenApi.OpenApi
mkOpenApi options (OpenApiRouter builders) label =
  runOpenApiGen options $ do
    builder <-
      case Map.lookup label (labeledBuilders builders) of
        Nothing -> failOpenApiGenOne (NoDefinitionForLabel label)
        Just builder -> pure builder

    apiInfo <-
      runOpenApiBuilder builder $
        PathInfo
          { pathInfoPath = ""
          , pathInfoParams = []
          }

    case DList.toList (lintApiInfo options apiInfo) of
      someErrors@(_ : _) ->
        failOpenApiGenMany someErrors
      [] -> do
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

lintApiInfo :: OpenApiOptions -> ApiInfo -> DList.DList OpenApiError
lintApiInfo options apiInfo =
  Map.foldMapWithKey
    (checkSchemaComponentEntry options)
    (apiSchemaComponents apiInfo)

checkSchemaComponentEntry :: OpenApiOptions -> T.Text -> SchemaInfo -> DList.DList OpenApiError
checkSchemaComponentEntry options schemaName schemaInfo =
  let
    allowedChars =
      openApiAllowedSchemaNameChars options
  in
    if T.all (flip Set.member allowedChars) schemaName
      then DList.empty
      else DList.singleton (InvalidSchemaName schemaName schemaInfo allowedChars)

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

combineApiInfo :: ApiInfo -> ApiInfo -> OpenApiGen ApiInfo
combineApiInfo left right = do
  components <-
    eitherToOpenApiGen $
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
  Either OpenApiError (Map.Map T.Text SchemaInfo)
combineSchemaComponents left right =
  let
    checkForConflict ::
      T.Text ->
      These.These SchemaInfo SchemaInfo ->
      Either OpenApiError SchemaInfo
    checkForConflict key theseSchemas =
      case theseSchemas of
        These.This this -> Right this
        These.That that -> Right that
        These.These this that ->
          case DList.toList (schemaConflicts this that) of
            [] -> pure this
            conflicts ->
              Left $
                SchemaConflict
                  key
                  this
                  that
                  conflicts
  in
    sequence (IAlign.ialignWith checkForConflict left right)

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

{- |
  Options can be specified to 'mkOpenApi' and 'mkAllOpenApis' to control the
  OpenApi construction. This type is exported without it's constructor. Use
  'defaultOpenApiOptions' to construct a value and use record update syntax
  to update whichever options you wish.

  See the record members below.
-}
data OpenApiOptions
  = OpenApiOptions
  { openApiAllowedSchemaNameChars :: Set.Set Char
  {- ^ Controls which characters are allowed in the names of schemas in the
  generated OpenAPI spec. By default this is set to @0-9A-Z_.a-z@ as this
  promotes better names in code that is from from the resulting OpenAPI spec
  -}
  }

{- |
  Sensible default options for generating OpenAPI specs. See the descriptions of
  each record field fold the default values.
-}
defaultOpenApiOptions :: OpenApiOptions
defaultOpenApiOptions =
  OpenApiOptions
    { openApiAllowedSchemaNameChars =
        Set.fromList
          ( -- Update the docs for openApiAllowedSchemaNameChars if you change this
            ['0' .. '9'] ++ ['A' .. 'Z'] ++ "_." ++ ['a' .. 'z']
          )
    }

newtype OpenApiGen a
  = OpenApiGen (Reader.ReaderT OpenApiOptions (Either [OpenApiError]) a)
  deriving newtype (Functor, Applicative, Monad)

failOpenApiGenOne :: OpenApiError -> OpenApiGen a
failOpenApiGenOne =
  failOpenApiGenMany . pure

failOpenApiGenMany :: [OpenApiError] -> OpenApiGen a
failOpenApiGenMany =
  OpenApiGen . Trans.lift . Left

data OpenApiError
  = InternalError String
  | NoDefinitionForLabel String
  | BeelineMethodUsed HTTPTypes.StdMethod PathInfo
  | UnsupportedMethod HTTPTypes.StdMethod PathInfo
  | InvalidSchemaName T.Text SchemaInfo (Set.Set Char)
  | SchemaConflict T.Text SchemaInfo SchemaInfo [String]

instance Show OpenApiError where
  show = renderOpenApiError

renderOpenApiError :: OpenApiError -> String
renderOpenApiError err =
  case err of
    InternalError msg ->
      "Internal Error: " <> msg
    NoDefinitionForLabel label ->
      "No OpenApi definition found with label " <> label <> "."
    BeelineMethodUsed method pathInfo ->
      "Unable to make OpenAPI description for router defined using standard Beeline 'method' (or helpers such as 'get'): "
        <> BS8.unpack (HTTPTypes.renderStdMethod method)
        <> " "
        <> pathInfoPath pathInfo
    UnsupportedMethod method pathInfo ->
      "Unable to create OpenAPI for description of "
        <> pathInfoPath pathInfo
        <> " due to use of HTTP Method: "
        <> BS8.unpack (HTTPTypes.renderStdMethod method)
    InvalidSchemaName schemaName schemaInfo allowedChars ->
      "Invalid Schema Name: "
        <> show schemaName
        <> " only the following characters are allowed, "
        <> show (Set.toList allowedChars)
        <> ".\n"
        <> renderPath (schemaPath schemaInfo)
    SchemaConflict key this that conflicts ->
      unlines $
        ("Conflicting schema definitions found for " <> T.unpack key)
          : "========= Left Side"
          : renderPath (schemaPath this)
          : "========= Right side"
          : renderPath (schemaPath that)
          : "========= Conflicts"
          : conflicts

runOpenApiGen :: OpenApiOptions -> OpenApiGen a -> Either [OpenApiError] a
runOpenApiGen options (OpenApiGen reader) =
  Reader.runReaderT reader options

eitherToOpenApiGen :: Either OpenApiError a -> OpenApiGen a
eitherToOpenApiGen = OpenApiGen . Trans.lift . Bifunctor.first pure

newtype OpenApiBuilder
  = OpenApiBuilder (PathInfo -> OpenApiGen ApiInfo)

runOpenApiBuilder :: OpenApiBuilder -> PathInfo -> OpenApiGen ApiInfo
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
  OpenApiBuilder (const (pure emptyApiInfo))

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
          mbRequestBody <- eitherToOpenApiGen $ mkRequestBody handler

          let
            (mbReqBody, reqComponents) =
              case mbRequestBody of
                Nothing -> (Nothing, Map.empty)
                Just (reqBody, reqComps) -> (Just reqBody, reqComps)

          (responses, responseComponents) <- eitherToOpenApiGen $ mkResponses handler
          allComponents <-
            eitherToOpenApiGen $
              combineSchemaComponents reqComponents responseComponents

          let
            operation =
              mempty
                { OpenApi._operationOperationId = Just . T.pack . Handler.handlerId $ handler
                , OpenApi._operationRequestBody = mbReqBody
                , OpenApi._operationResponses = responses
                , OpenApi._operationParameters = mkQueryParams handler <> mkHeaderParams handler
                }

            pathInfo =
              mkRoute parentContext

            mbPathItem =
              mkPathItem method pathInfo operation

          case mbPathItem of
            Nothing ->
              failOpenApiGenOne (UnsupportedMethod method pathInfo)
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
              failOpenApiGenOne (BeelineMethodUsed method pathInfo)
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
  Either OpenApiError (Maybe (OpenApi.Referenced OpenApi.RequestBody, Map.Map T.Text SchemaInfo))
mkRequestBody handler =
  case Handler.requestBody handler of
    Handler.SchemaRequestBody (FC.Schema _ (FleeceOpenApi mkErrOrSchemaInfo)) -> do
      schemaInfo <- mkErrOrSchemaInfo []

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
    Handler.RawRequestBody _decoder ->
      Right Nothing
    Handler.FormDataRequestBody _formDecoder ->
      Right Nothing
    Handler.EmptyRequestBody ->
      Right Nothing

mkQueryParams ::
  Handler.Handler route ->
  [OpenApi.Referenced OpenApi.Param]
mkQueryParams handler =
  case Handler.requestQuery handler of
    Handler.EmptyRequestQuery ->
      []
    Handler.RequestQuery schema ->
      toOpenApiParams schema OpenApi.ParamQuery

mkHeaderParams ::
  Handler.Handler route ->
  [OpenApi.Referenced OpenApi.Param]
mkHeaderParams handler =
  case Handler.requestHeaders handler of
    Handler.EmptyRequestHeaders ->
      []
    Handler.RequestHeaders schema ->
      toOpenApiParams schema OpenApi.ParamHeader

newtype OpenApiParams record a = OpenApiParams
  { toOpenApiParamsDList :: OpenApi.ParamLocation -> DList.DList (OpenApi.Referenced OpenApi.Param)
  }

toOpenApiParams ::
  OpenApiParams record a ->
  OpenApi.ParamLocation ->
  [OpenApi.Referenced OpenApi.Param]
toOpenApiParams params =
  DList.toList . toOpenApiParamsDList params

instance BP.ParameterSchema OpenApiParams where
  newtype Parameter OpenApiParams query a
    = OpenApiItem (OpenApi.ParamLocation -> DList.DList (OpenApi.Referenced OpenApi.Param))

  makeParams _constructor =
    OpenApiParams (const DList.empty)

  validateParams _unvalidate _validate (OpenApiParams params) =
    OpenApiParams params

  addParam (OpenApiParams params) (OpenApiItem moreParams) =
    -- TODO detect conflicts, keep order? always sort lexictally?
    OpenApiParams (params <> moreParams)

  required _accessor paramDef =
    OpenApiItem
      ( DList.singleton
          . OpenApi.Inline
          . mkOpenApiScalarParam True paramDef
      )

  optional _accessor paramDef =
    OpenApiItem
      ( DList.singleton
          . OpenApi.Inline
          . mkOpenApiScalarParam False paramDef
      )

  splat _accessor (OpenApiParams f) =
    OpenApiItem f

instance BP.QuerySchema OpenApiParams where
  explodedArray _accessor paramDef =
    OpenApiItem
      ( DList.singleton
          . OpenApi.Inline
          . mkOpenApiExplodedArrayParam False paramDef
      )

  explodedNonEmpty _accessor paramDef =
    OpenApiItem
      ( DList.singleton
          . OpenApi.Inline
          . mkOpenApiExplodedArrayParam True paramDef
      )

instance BP.HeaderSchema OpenApiParams where
  type Cookies OpenApiParams = OpenApiParams

  cookies _accessor cookieParams =
    OpenApiItem $ \_location ->
      toOpenApiParamsDList cookieParams OpenApi.ParamCookie

mkResponses ::
  Handler.Handler router ->
  Either OpenApiError (OpenApi.Responses, Map.Map T.Text SchemaInfo)
mkResponses handler =
  let
    schemas =
      Response.responseBodyList . Handler.handlerResponseBodies $ handler

    addResponse (responses, components) (status, responseSchema) = do
      mbSchemaInfo <-
        case responseSchema of
          Response.NoSchemaResponseBody _mbContentType -> pure Nothing
          Response.SchemaResponseBody (FC.Schema _ (FleeceOpenApi mkInfo)) -> fmap Just (mkInfo [])
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

mkOpenApiScalarParam ::
  Bool ->
  R.ParameterDefinition a ->
  OpenApi.ParamLocation ->
  OpenApi.Param
mkOpenApiScalarParam isRequired param location =
  mempty
    { OpenApi._paramName = R.parameterName param
    , OpenApi._paramIn = location
    , OpenApi._paramRequired = Just isRequired
    , OpenApi._paramSchema = Just (mkParamSchema param)
    }

mkOpenApiExplodedArrayParam ::
  Bool ->
  R.ParameterDefinition a ->
  OpenApi.ParamLocation ->
  OpenApi.Param
mkOpenApiExplodedArrayParam isRequired param location =
  let
    itemSchema =
      mkParamSchema param
  in
    mempty
      { OpenApi._paramName = R.parameterName param
      , OpenApi._paramIn = location
      , OpenApi._paramRequired = Just isRequired
      , OpenApi._paramStyle = Just OpenApi.StyleForm
      , OpenApi._paramExplode = Just True
      , OpenApi._paramSchema =
          Just
            . OpenApi.Inline
            $ mempty
              { OpenApi._schemaType = Just OpenApi.OpenApiArray
              , OpenApi._schemaItems = Just (OpenApi.OpenApiItemsObject itemSchema)
              }
      }

mkParamSchema :: R.ParameterDefinition param -> OpenApi.Referenced OpenApi.Schema
mkParamSchema param =
  let
    schemaType =
      case R.parameterType param of
        R.ParameterString -> OpenApi.OpenApiString
        R.ParameterNumber -> OpenApi.OpenApiNumber
        R.ParameterInteger -> OpenApi.OpenApiInteger
        R.ParameterBoolean -> OpenApi.OpenApiBoolean
  in
    OpenApi.Inline $
      mempty
        { OpenApi._schemaType = Just schemaType
        , OpenApi._schemaFormat = R.parameterFormat param
        }

{- |
  A concrete type that implements the Fleece typeclasses to build an
  OpenApi description of the JSON describe by the Fleece schema definition.
  Can be used with 'schemaWithComponents' to retrieve the 'OpenApi.Schema'
  description of the schema.
-}
newtype FleeceOpenApi a = FleeceOpenApi
  { unFleeceOpenApi :: Path -> Either OpenApiError SchemaInfo
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
schemaWithComponents :: FC.Schema FleeceOpenApi a -> Either OpenApiError SchemaWithComponents
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
    . ($ [])
    . unFleeceOpenApi
    . FC.schemaInterpreter

data PathEntry
  = PathSchema FC.Name
  | PathField FC.Name String
  deriving (Show)

renderPathEntry :: PathEntry -> String
renderPathEntry pathEntry =
  case pathEntry of
    PathSchema schemaName -> "Schema " <> FC.nameToString schemaName
    PathField schemaName field -> FC.nameToString schemaName <> "." <> field

type Path = [PathEntry]

addSchemaToPath :: FC.Name -> Path -> Path
addSchemaToPath =
  (:) . PathSchema

addFieldToPath :: String -> Path -> Path
addFieldToPath field path =
  case path of
    [] -> [PathField topLevelDummySchemaName field]
    (PathSchema schemaName : rest) -> PathField schemaName field : rest
    (PathField schemaName oldField : rest) -> PathField schemaName (oldField <> "." <> field) : rest

topLevelDummySchemaName :: FC.Name
topLevelDummySchemaName = FC.unqualifiedName "<<toplevel>>"

renderPath :: Path -> String
renderPath path =
  case path of
    [] -> renderPath [PathSchema topLevelDummySchemaName]
    (first : rest) ->
      let
        render :: String -> PathEntry -> String
        render label pathEntry =
          "  - " <> label <> ": " <> renderPathEntry pathEntry
      in
        unlines $
          (render "Found at" first)
            : map (render "Within") rest

data SchemaInfo = SchemaInfo
  { fleeceName :: FC.Name
  , schemaPath :: Path
  , schemaIsPrimitive :: Bool
  , openApiKey :: Maybe T.Text
  , openApiNullable :: Bool
  , openApiSchema :: OpenApi.Schema
  , schemaComponents :: Map.Map T.Text SchemaInfo
  }
  deriving (Show)

isArraySchemaInfo :: SchemaInfo -> Bool
isArraySchemaInfo =
  (== Just OpenApi.OpenApiArray) . OpenApi._schemaType . openApiSchema

schemaConflicts :: SchemaInfo -> SchemaInfo -> DList.DList String
schemaConflicts
  (SchemaInfo fleeceName1 _path1 schemaIsPrimitive1 openApiKey1 _nullable1 openApiSchema1 schemaComponents1)
  (SchemaInfo fleeceName2 _path2 schemaIsPrimitive2 openApiKey2 _nullable2 openApiSchema2 schemaComponents2) =
    eqConflict "Fleece Name" fleeceName1 fleeceName2
      <> eqConflict "Is Primitive" schemaIsPrimitive1 schemaIsPrimitive2
      <> eqConflict "OpenAPI Key" openApiKey1 openApiKey2
      <> eqConflict "OpenAPI Schema" openApiSchema1 openApiSchema2
      <> foldMap
        id
        ( Align.alignWith
            ( These.these
                (\leftSchema -> DList.singleton (show (fleeceName leftSchema) <> " only present in first"))
                (\rightSchema -> DList.singleton (show (fleeceName rightSchema) <> " only present in second"))
                schemaConflicts
            )
            schemaComponents1
            schemaComponents2
        )

eqConflict ::
  (Eq a, Show a) =>
  String ->
  a ->
  a ->
  DList.DList String
eqConflict name left right =
  if left == right
    then DList.empty
    else DList.singleton (name <> ": " <> show left <> " /= " <> show right)

setSchemaInfoFormat :: T.Text -> SchemaInfo -> SchemaInfo
setSchemaInfoFormat fmt info =
  info
    { openApiSchema =
        (openApiSchema info)
          { OpenApi._schemaFormat = Just fmt
          }
    }

setOpenApiType :: OpenApi.OpenApiType -> FleeceOpenApi a -> FleeceOpenApi a
setOpenApiType typ (FleeceOpenApi mkErrOrSchemaInfo) = do
  FleeceOpenApi $ \path -> do
    schemaInfo <- mkErrOrSchemaInfo path
    pure
      schemaInfo
        { openApiSchema =
            (openApiSchema schemaInfo)
              { OpenApi._schemaType = Just typ
              }
        }

collectComponents :: [SchemaInfo] -> Either OpenApiError (Map.Map T.Text SchemaInfo)
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
      let
        ref = OpenApi.Ref . OpenApi.Reference $ schemaKey
      in
        if openApiNullable schema
          then
            OpenApi.Inline $
              mempty
                { OpenApi._schemaOneOf = Just [ref]
                , OpenApi._schemaNullable = Just True
                }
          else ref
    Nothing ->
      OpenApi.Inline . openApiSchema $ schema

mkPrimitiveSchema ::
  FC.Name ->
  OpenApi.OpenApiType ->
  Path ->
  SchemaInfo
mkPrimitiveSchema name openApiType path =
  SchemaInfo
    { fleeceName = name
    , schemaPath = path
    , schemaIsPrimitive = True
    , openApiKey = Nothing
    , openApiNullable = False
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
    = Object (Path -> Either OpenApiError [FieldInfo])

  data Field FleeceOpenApi _object _field
    = Field (Path -> Either OpenApiError FieldInfo)

  data AdditionalFields FleeceOpenApi _object _field
    = AdditionalFields

  data UnionMembers FleeceOpenApi _allTypes _handledTypes
    = UnionMembers (Path -> Either OpenApiError [SchemaInfo])

  data TaggedUnionMembers FleeceOpenApi _allTags _handledTags
    = TaggedUnionMembers (Path -> FieldInfo -> String -> Either OpenApiError [(T.Text, SchemaInfo)])

  interpretFormat formatString (FC.Schema _name (FleeceOpenApi mkErrOrSchemaInfo)) =
    FleeceOpenApi $ fmap (setSchemaInfoFormat (T.pack formatString)) . mkErrOrSchemaInfo

  interpretNumber name =
    FleeceOpenApi $ Right . mkPrimitiveSchema name OpenApi.OpenApiNumber

  interpretText name =
    FleeceOpenApi $ Right . mkPrimitiveSchema name OpenApi.OpenApiString

  interpretBoolean name =
    FleeceOpenApi $ Right . mkPrimitiveSchema name OpenApi.OpenApiBoolean

  interpretNull name =
    FleeceOpenApi $ Right . mkPrimitiveSchema name OpenApi.OpenApiNull

  interpretArray arrayName (FC.Schema _itemName (FleeceOpenApi mkErrOrItemSchemaInfo)) =
    FleeceOpenApi $ \path -> do
      itemSchemaInfo <- mkErrOrItemSchemaInfo path
      components <- collectComponents [itemSchemaInfo]

      let
        itemSchemaRef =
          mkSchemaRef itemSchemaInfo

      pure $
        SchemaInfo
          { fleeceName = arrayName
          , schemaPath = path
          , schemaIsPrimitive = False
          , openApiKey = Nothing
          , openApiNullable = False
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Just OpenApi.OpenApiArray
                , OpenApi._schemaItems = Just (OpenApi.OpenApiItemsObject itemSchemaRef)
                }
          , schemaComponents = components
          }

  interpretNullable _nullableName (FC.Schema _name (FleeceOpenApi mkErrOrSchemaInfo)) =
    FleeceOpenApi $ \path -> do
      schemaInfo <- mkErrOrSchemaInfo path
      let
        innerSchemaShouldBeNullable =
          (schemaIsPrimitive schemaInfo || isArraySchemaInfo schemaInfo)
            && Maybe.isNothing (openApiKey schemaInfo)

      pure $
        SchemaInfo
          { fleeceName = fleeceName schemaInfo
          , schemaPath = path
          , schemaIsPrimitive = schemaIsPrimitive schemaInfo
          , openApiKey = openApiKey schemaInfo
          , openApiNullable = True
          , openApiSchema =
              (openApiSchema schemaInfo)
                { OpenApi._schemaNullable =
                    if innerSchemaShouldBeNullable
                      then Just True
                      else Nothing
                }
          , schemaComponents = schemaComponents schemaInfo
          }

  required name _accessor (FC.Schema _schemaName (FleeceOpenApi mkErrOrSchemaInfo)) =
    Field $ \path -> do
      schemaInfo <- mkErrOrSchemaInfo (addFieldToPath name path)
      pure $
        FieldInfo
          { fieldName = T.pack name
          , fieldRequired = True
          , fieldSchemaInfo = schemaInfo
          }

  optional name _accessor (FC.Schema _schemaName (FleeceOpenApi mkErrOrSchemaInfo)) =
    Field $ \path -> do
      schemaInfo <- mkErrOrSchemaInfo (addFieldToPath name path)
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

  interpretObjectNamed name (Object mkErrOrFieldsInReverse) =
    FleeceOpenApi $ \path ->
      mkObjectForFields path name =<< mkErrOrFieldsInReverse (addSchemaToPath name path)

  constructor _cons =
    Object . const . Right $ []

  field (Object mkFieldInfos) (Field mkNewFieldInfo) =
    Object (\path -> liftA2 (:) (mkNewFieldInfo path) (mkFieldInfos path))

  additional (Object _fields) _additional =
    Object . const . Left . InternalError $
      "Fleece additional fields not currently support for OpenAPI"

  interpretValidateNamed name _uncheck _check (FC.Schema _unvalidatedName (FleeceOpenApi mkErrOrSchemaInfo)) = do
    FleeceOpenApi $ \path -> do
      schemaInfo <- mkErrOrSchemaInfo (addSchemaToPath name path)

      if schemaIsPrimitive schemaInfo
        then do
          components <- collectComponents [schemaInfo]
          pure $
            schemaInfo
              { fleeceName = name
              , openApiKey = Just . fleeceNameToOpenApiKey $ name
              , openApiNullable = False
              , schemaComponents = components
              }
        else
          pure $
            schemaInfo
              { fleeceName = name
              , openApiKey = Just . fleeceNameToOpenApiKey $ name
              }

  interpretValidateAnonymous _uncheck _check (FC.Schema _name (FleeceOpenApi errOrSchemaInfo)) = do
    FleeceOpenApi errOrSchemaInfo

  interpretBoundedEnumNamed name toText =
    let
      enumValues =
        fmap
          (Aeson.toJSON . toText)
          [minBound .. maxBound]
    in
      FleeceOpenApi $ \path ->
        Right $
          SchemaInfo
            { fleeceName = name
            , schemaPath = path
            , schemaIsPrimitive = False
            , openApiKey = Just . fleeceNameToOpenApiKey $ name
            , openApiNullable = False
            , openApiSchema =
                mempty
                  { OpenApi._schemaType = Just OpenApi.OpenApiString
                  , OpenApi._schemaEnum = Just enumValues
                  }
            , schemaComponents = Map.empty
            }

  interpretUnionNamed name (UnionMembers mkErrOrMembers) =
    FleeceOpenApi $ \path -> do
      let
        key = Just $ fleeceNameToOpenApiKey name

      members <- mkErrOrMembers (PathSchema name : path)
      components <- collectComponents members

      pure $
        SchemaInfo
          { fleeceName = name
          , schemaPath = path
          , schemaIsPrimitive = False
          , openApiKey = key
          , openApiNullable = False
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

  unionMemberWithIndex _idx (FC.Schema _name (FleeceOpenApi mkErrOrSchemaInfo)) =
    UnionMembers $ \path -> do
      schemaInfo <- mkErrOrSchemaInfo path
      pure [schemaInfo]

  unionCombine (UnionMembers left) (UnionMembers right) =
    -- Don't change this to '<>' or we might get bitten by accidental
    -- polymorphism
    UnionMembers $ \path -> liftA2 (++) (left path) (right path)

  interpretTaggedUnionNamed name tagPropertyString (TaggedUnionMembers mkMembers) =
    FleeceOpenApi $ \path -> do
      let
        tagProperty =
          T.pack tagPropertyString

        memberKeyPrefix =
          FC.nameUnqualified name <> "."

        errOrStringSchema =
          unFleeceOpenApi (FC.schemaInterpreter FC.text) (PathSchema name : path)

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
              Left . InternalError $
                "No Schema Key found for member "
                  <> T.unpack tagValue
                  <> " of union "
                  <> FC.nameToString name

      stringSchema <- errOrStringSchema
      members <- mkMembers (PathSchema name : path) (mkTagField stringSchema) memberKeyPrefix

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
          , schemaPath = path
          , schemaIsPrimitive = False
          , openApiKey = key
          , openApiNullable = False
          , openApiSchema =
              mempty
                { OpenApi._schemaType = Nothing
                , OpenApi._schemaDiscriminator = Just discriminator
                , OpenApi._schemaOneOf = Just memberSchemaRefs
                , OpenApi._schemaTitle = key
                }
          , schemaComponents = components
          }

  taggedUnionMemberWithTag tag (Object mkErrOrFieldsInReverse) =
    TaggedUnionMembers $ \path tagField memberKeyPrefix -> do
      let
        tagValue =
          symbolVal tag

        memberName =
          FC.unqualifiedName (memberKeyPrefix <> tagValue)

      fieldsInReverse <- mkErrOrFieldsInReverse path

      let
        fieldsInReverseWithTag =
          fieldsInReverse <> [tagField]

      objectSchema <- mkObjectForFields path memberName fieldsInReverseWithTag

      pure [(T.pack tagValue, objectSchema)]

  taggedUnionCombine (TaggedUnionMembers mkLeft) (TaggedUnionMembers mkRight) =
    TaggedUnionMembers $ \path tagProperty memberKeyPrefix -> do
      left <- mkLeft path tagProperty memberKeyPrefix
      right <- mkRight path tagProperty memberKeyPrefix
      -- Don't change this to '<>' or we might get bitten by accidental
      -- polymorphism
      pure (left ++ right)

  interpretJsonString _schema =
    FleeceOpenApi
      . const
      . Left
      . InternalError
      $ "Fleece jsonString is not currently implemented for OpenApi"

  --
  -- Default implementations we override to get OpenAPI specific behavior.
  -- Unfortunately this requires that we duplicate the default implementations
  -- of these members from the class implementations in json-fleece because
  -- we have no way to access and call the defaults.
  --

  interpretInt _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter FC.boundedIntegralNumberAnonymous)

  interpretInt8 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "int8" FC.boundedIntegralNumberAnonymous)

  interpretInt16 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "int16" FC.boundedIntegralNumberAnonymous)

  interpretInt32 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "int32" FC.boundedIntegralNumberAnonymous)

  interpretInt64 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "int64" FC.boundedIntegralNumberAnonymous)

  interpretWord _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "word" FC.boundedIntegralNumberAnonymous)

  interpretWord8 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "word8" FC.boundedIntegralNumberAnonymous)

  interpretWord16 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "word16" FC.boundedIntegralNumberAnonymous)

  interpretWord32 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "word32" FC.boundedIntegralNumberAnonymous)

  interpretWord64 _name =
    setOpenApiType OpenApi.OpenApiInteger (FC.schemaInterpreter $ FC.format "word64" FC.boundedIntegralNumberAnonymous)

mkObjectForFields ::
  Path ->
  FC.Name ->
  [FieldInfo] ->
  Either OpenApiError SchemaInfo
mkObjectForFields path name fieldsInReverse = do
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
      , schemaPath = path
      , schemaIsPrimitive = False
      , openApiKey = key
      , openApiNullable = False
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

componentsPrefix :: T.Text
componentsPrefix =
  "#/components/schemas/"
