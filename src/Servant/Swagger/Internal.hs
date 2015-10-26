{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
#if !MIN_VERSION_base(4,8,0)
  {-# LANGUAGE OverlappingInstances   #-}
#endif

------------------------------------------------------------------------------
module Servant.Swagger.Internal  where
------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.Aeson
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Data
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import           Data.Bool
import           GHC.Generics
import           Data.String
import           Control.Lens hiding ((.=))
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           GHC.TypeLits
import           Servant.API hiding (Header)
import qualified Servant.API.Header as H

data SwagRoute = SwagRoute {
    _routePathName    :: PathName
  , _routeConsumes    :: [ContentType]
  , _routeModels      :: H.HashMap ModelName SwaggerModel
  , _routeParams      :: [Param]
  , _routeVerb        :: Verb
  , _routePathSummary :: PathSummary
  , _routeRespHeaders :: H.HashMap Text ResponseHeader
  } deriving Show

defSwagRoute :: SwagRoute
defSwagRoute = SwagRoute (PathName "") [] [] [] Get mempty [] 

defSwaggerInfo :: Info
defSwaggerInfo =
  Info (APITitle mempty)
    (APIVersion "2.0") (APIDescription mempty) Nothing

newtype APIDescription = APIDescription { _unApiDesc :: Text }
   deriving (Show, Eq, ToJSON)

newtype APITermsOfService = APITermsOfService { _unAPITermsOfService :: Text }
   deriving (Show, Eq, ToJSON)

data ResponseHeader = ResponseHeader {
    responseHeaderDescription :: Text
  , responseHeaderType :: SwaggerParamType
  , responseHeaderName :: Text
  } deriving (Show, Eq)

data Response = Response {
     _description :: Text
  , _responseModelName :: ModelName
  , _responseHeaders :: H.HashMap Text ResponseHeader
  , _responseIsArray :: Bool
  } deriving (Show, Eq)

newtype Tag = Tag Text
  deriving (Show, Eq, IsString, Ord, ToJSON, FromJSON)

data SwaggerAPI = SwaggerAPI {
     _swaggerInfo        :: Info
  ,  _swaggerSchemes     :: [Scheme]
  ,  _swaggerPaths       :: H.HashMap PathName SwaggerPath
  ,  _swaggerDefinitions :: H.HashMap ModelName SwaggerModel
  ,  _swaggerTags        :: [Tag]
  ,  _swaggerBasePath    :: BasePath
  } deriving Show

newtype BasePath = BasePath Text
   deriving (Show, Eq, ToJSON, FromJSON)

data Info = Info {
    _swaggerInfoTitle      :: APITitle
  , _swaggerVersion        :: APIVersion
  , _swaggerAPIDescription :: APIDescription
  , _license               :: Maybe APILicense
  } deriving (Show, Eq)

data APILicense = APILicense {
     _licenseName :: Text
  ,  _licenseUrl  :: Maybe Text
  } deriving (Show, Eq)

data SwaggerPath = SwaggerPath {
     _paths :: H.HashMap Verb Path
  } deriving Show

data SwagResult = SwagResult {
    _resultPaths  :: H.HashMap PathName SwaggerPath
  , _resultModels :: H.HashMap ModelName SwaggerModel
  } deriving (Show)

data Verb = Post | Get | Put | Options | Head | Delete | Patch
  deriving (Show, Eq, Read, Generic)

newtype PathSummary = PathSummary Text
    deriving (Show, Eq, ToJSON, FromJSON, Monoid, IsString)

data Path = Path {
     _summary   :: PathSummary
   , _params    :: [Param]
   , _responses :: H.HashMap Code Response
   , _produces  :: [ContentType]
   , _consumes  :: [ContentType]
   , _tags      :: [Tag]
  } deriving Show

newtype Code = Code Int deriving (Show, Eq, Ord, ToJSON, Hashable, Num)

data SwaggerParamType =
    StringSwagParam
  | NumberSwagParam
  | IntegerSwagParam
  | BooleanSwagParam
  | ArraySwagParam
  | FileSwagParam
  deriving (Show, Eq)

data SwaggerType =
    IntegerSwag
  | LongSwag
  | FloatSwag
  | DoubleSwag
  | StringSwag
  | ByteSwag
  | BinarySwag
  | BooleanSwag
  | DateSwag
  | DateTimeSwag
  | PasswordSwag
  | Model ModelSwag
  deriving (Show, Eq)

data ModelSwag = ModelSwag {
    modelSwagName :: ModelName
  , modelSwagIsArray :: Bool
  } deriving (Show, Eq)

data ContentType = JSON | HTML | XML | FormUrlEncoded | PlainText | OctetStream
  deriving (Show, Eq)

data In = PathUrl | Query | Header | FormData | Body deriving Show
data Scheme = Http | Https | Ws | Wss deriving Show

data Param = Param {
    _in               :: In
  , _name             :: Text
  , _type             :: Maybe SwaggerParamType
  , _items            :: Maybe ItemObject
  , _paramDescription :: Text
  , _allowEmptyValue  :: Bool
  , _required         :: Bool
  , _default          :: Maybe Value
  , _isArray          :: Bool
  } deriving Show

data ItemObject = ItemObject {
     _itemsType :: SwaggerParamType
  } deriving Show

newtype APIVersion = APIVersion Text deriving (Show, Eq, ToJSON)
newtype APITitle = APITitle Text deriving (Show, Eq, ToJSON)
newtype PathName = PathName { unPathName :: Text }
  deriving (Show, Eq, Hashable, Monoid)

newtype ModelName = ModelName { unModelName :: Text } deriving (Show, Eq, Hashable, Monoid)
newtype Description = Description { unDescription :: Text } deriving (Show, Eq, ToJSON, Monoid)

data SwaggerModel = SwaggerModel {
     _swagModelName    :: ModelName
   , _swagProperties   :: [(Text, SwaggerType)]
   , _swagDescription  :: Maybe Description
   , _swagModelExample :: Maybe Value
   , _swagModelRequired :: [Text]
   } deriving (Show, Eq)

emptySwaggerModel :: SwaggerModel
emptySwaggerModel = SwaggerModel (ModelName mempty) mempty mempty Nothing mempty

$(makeLenses ''SwaggerModel)
$(makeLenses ''SwagResult)
$(makeLenses ''SwagRoute)
$(makeLenses ''SwaggerAPI)
$(makeLenses ''Info)
$(makeLenses ''APILicense)

------------------------------------------------------------------------------
-- | Swaggin'
class HasSwagger h where
  toSwaggerDocs :: Proxy h -> SwagRoute -> SwagResult
------------------------------------------------------------------------------
class ToSwaggerDescription a where toSwaggerDescription :: Proxy a -> Text
class ToResponseHeader a where toResponseHeader :: Proxy a -> ResponseHeader
class ToResponseHeaders as where toResponseHeaders :: Proxy as -> [ ResponseHeader ]
instance ToResponseHeaders '[] where toResponseHeaders Proxy = []

------------------------------------------------------------------------------
instance (HasSwagger rest, KnownSymbol sym) => HasSwagger (sym :> rest) where
  toSwaggerDocs Proxy swagRoute =
     toSwaggerDocs (Proxy :: Proxy rest) $ swagRoute & routePathName %~ flip (<>) path
   where path = PathName $ "/" <> T.pack (symbolVal (Proxy :: Proxy sym))

instance (HasSwagger left, HasSwagger right) => HasSwagger (left :<|> right) where
  toSwaggerDocs Proxy swagRoute =
    let swagLeft = toSwaggerDocs (Proxy :: Proxy left) swagRoute
        swagRight = toSwaggerDocs (Proxy :: Proxy right) swagRoute
        paths  = H.unionWith f (swagLeft ^. resultPaths) (swagRight ^. resultPaths)
        models = H.union (swagLeft ^. resultModels) (swagRight ^. resultModels)
    in SwagResult paths models 
      where f (SwaggerPath l) (SwaggerPath r) = SwaggerPath (H.union l r)

class ToSwaggerParamType a where toSwaggerParamType :: Proxy a -> SwaggerParamType
instance ToSwaggerParamType Int where toSwaggerParamType = const IntegerSwagParam
instance ToSwaggerParamType String where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Text where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType L.Text where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType BL8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType B8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Double where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Float where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Bool where toSwaggerParamType Proxy = BooleanSwagParam
instance ToSwaggerParamType a => ToSwaggerParamType [a] where
  toSwaggerParamType Proxy = ArraySwagParam

instance ( ToSwaggerParamType headerType
         , KnownSymbol headerName
         ) => ToResponseHeader (H.Header headerName headerType) where
  toResponseHeader Proxy = ResponseHeader mempty ht hn
      where
        hn = T.pack . symbolVal $ (Proxy :: Proxy headerName)
        ht = toSwaggerParamType (Proxy :: Proxy headerType)  

class SwaggerAccept a where toSwaggerAccept :: Proxy a -> ContentType
instance SwaggerAccept JSON where toSwaggerAccept Proxy = JSON
instance SwaggerAccept 'JSON where toSwaggerAccept Proxy = JSON
instance SwaggerAccept 'HTML where toSwaggerAccept Proxy = HTML
instance SwaggerAccept 'XML where toSwaggerAccept Proxy = XML
instance SwaggerAccept 'FormUrlEncoded where toSwaggerAccept Proxy = FormUrlEncoded
instance SwaggerAccept 'PlainText where toSwaggerAccept Proxy = PlainText
instance SwaggerAccept 'OctetStream where toSwaggerAccept Proxy = OctetStream
------------------------------------------------------------------------------
class SwaggerAcceptTypes (xs :: [*]) where toSwaggerAcceptTypes :: Proxy xs -> [ContentType]
instance SwaggerAcceptTypes '[] where toSwaggerAcceptTypes Proxy = []
instance (SwaggerAccept x, SwaggerAcceptTypes xs) => SwaggerAcceptTypes (x ': xs) where
  toSwaggerAcceptTypes Proxy =
    toSwaggerAccept (Proxy :: Proxy x) : toSwaggerAcceptTypes (Proxy :: Proxy xs)
------------------------------------------------------------------------------
class ToVerb a where toVerb :: Proxy a -> Verb
instance ToVerb Get where toVerb Proxy   = Get
instance ToVerb 'Get where toVerb Proxy   = Get
instance ToVerb Put where toVerb Proxy   = Put
instance ToVerb 'Put where toVerb Proxy   = Put
instance ToVerb Patch where toVerb Proxy = Patch
instance ToVerb 'Patch where toVerb Proxy = Patch
instance ToVerb 'Head where toVerb Proxy  = Head
instance ToVerb Post where toVerb Proxy  = Post
instance ToVerb 'Post where toVerb Proxy  = Post
instance ToVerb Delete where toVerb Proxy  = Delete
instance ToVerb 'Delete where toVerb Proxy  = Delete
instance ToVerb 'Options where toVerb Proxy  = Options

class ToSwaggerPathSummary a where
  toSwaggerPathSummary :: Proxy a -> PathSummary

class ToSwaggerModel a where 
  toSwagModel  :: Proxy a -> SwaggerModel
  toSwagModelName :: Proxy a -> ModelName
  toSwagModelName = _swagModelName . toSwagModel

instance ToSwaggerModel () where
  toSwagModel Proxy = emptySwaggerModel

instance ToSwaggerModel SwaggerAPI where
  toSwagModel Proxy = emptySwaggerModel

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs) 
      => HasSwagger (verb xs returnType) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path (swagRoute ^. routePathSummary) (swagRoute ^. routeParams)
            [(200, Response "OK" (swagModel ^. swagModelName) (swagRoute ^. routeRespHeaders) False)]
               (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes) []
    in SwagResult [(pathName, swagPath)] newModels
      where
        pathName = if (swagRoute ^. routePathName) == PathName ""
                   then PathName "/"
                   else swagRoute ^. routePathName
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = bool (swagRoute ^. routeModels)
                         (H.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptySwaggerModel)
instance
#if MIN_VERSION_base(4,8,0)
   {-# OVERLAPPING #-}
#endif
 (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs)
    => HasSwagger (verb xs [returnType]) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path (swagRoute ^. routePathSummary) (swagRoute ^. routeParams)
            [(200, Response "OK" (swagModel ^. swagModelName) (swagRoute ^. routeRespHeaders) True)]
               (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes) []
    in SwagResult [(swagRoute ^. routePathName, swagPath)] newModels
      where
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = bool (swagRoute ^. routeModels)
                         (H.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptySwaggerModel)

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (Capture sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newPath = PathName $ mconcat ["/{",pName,"}"]
        newParam = Param PathUrl pName
                     (Just $ toSwaggerParamType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True True Nothing False
        newSwagRoute = swagRoute & routePathName %~ flip (<>) newPath
                                 & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParam sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just $ toSwaggerParamType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing False
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParams sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just ArraySwagParam) (Just $ ItemObject (toSwaggerParamType (Proxy :: Proxy typ)))
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing True
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Query Flag
instance (ToSwaggerDescription sym, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryFlag sym :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just StringSwagParam) Nothing
                       (toSwaggerDescription (Proxy :: Proxy sym)) True False Nothing False
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Raw holds no verb / body information
instance HasSwagger Raw where
  toSwaggerDocs Proxy swagRoute =
    SwagResult [(swagRoute ^. routePathName, mempty)] [] 

------------------------------------------------------------------------------
-- | Swagger doesn't support Raw, bypass
instance HasSwagger rest => HasSwagger (MatrixParam typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) swagRoute

------------------------------------------------------------------------------
-- | Swagger doesn't support matrix flags, bypass
instance (ToSwaggerDescription typ, ToSwaggerParamType typ, HasSwagger rest) =>
  HasSwagger (MatrixFlag typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) swagRoute

------------------------------------------------------------------------------
-- | Swagger Header
instance (KnownSymbol sym, ToSwaggerDescription typ, ToSwaggerParamType typ, HasSwagger rest) =>
  HasSwagger (H.Header sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        newSwagRoute = swagRoute & routeParams %~ (:) newParams
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        pDesc = toSwaggerDescription (Proxy :: Proxy typ)
        typ = toSwaggerParamType (Proxy :: Proxy typ)
        newParams = Param Header pName (Just typ)
                       Nothing pDesc False True Nothing False

------------------------------------------------------------------------------
-- | ReqBody Object
instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
  HasSwagger (ReqBody ctypes model :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
         swagModel@SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwagRoute =
           swagRoute & routeModels %~ model
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         model | swagModel == emptySwaggerModel = (<> mempty)
               | otherwise = H.insert _swagModelName (toSwagModel (Proxy :: Proxy model))
         newParam =
            case _swagModelName of
              (ModelName "") -> []
              name -> [ Param Body (unModelName name) Nothing Nothing
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing False]

------------------------------------------------------------------------------
-- | ReqBody Array
instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
  (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
  HasSwagger (ReqBody ctypes [model] :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
         swagModel@SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwagRoute =
           swagRoute & routeModels %~ model
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         model | swagModel == emptySwaggerModel = (<> mempty)
               | otherwise = H.insert _swagModelName (toSwagModel (Proxy :: Proxy model))
         newParam =
            case _swagModelName of
              (ModelName "") -> []
              name -> [ Param Body (unModelName name) Nothing Nothing
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing True]

class ToModelExample model where toExample :: Proxy model -> Maybe Value

instance ToJSON ResponseHeader where
  toJSON ResponseHeader{..} = 
    object [
         "type" .= responseHeaderType
      ,  "description" .= responseHeaderDescription
      ]

instance Monoid SwaggerPath where
  mempty = SwaggerPath H.empty
  SwaggerPath a `mappend` SwaggerPath b =
    SwaggerPath ( a <> b )

instance ToJSON APILicense where
  toJSON APILicense{..} =
    object [ "name" .= _licenseName
           , "url"  .= _licenseUrl
           ]

instance Hashable Verb where hash = hash . show

instance ToJSON SwaggerParamType where
  toJSON StringSwagParam = String "string"
  toJSON NumberSwagParam = String "number"
  toJSON IntegerSwagParam = String "integer"
  toJSON BooleanSwagParam = String "boolean"
  toJSON ArraySwagParam = String "array"
  toJSON FileSwagParam = String "file"

instance ToJSON SwaggerType where
  toJSON x =
    let f typ format = object $ [ "type" .= (typ :: Text) ] ++
               if isJust format
                 then [ "format" .= ((fromJust format) :: Text) ]
                 else []
    in case x of
      IntegerSwag -> f "integer" (Just "int32")
      LongSwag -> f "integer" (Just "int64")
      FloatSwag -> f "number" (Just "float")
      DoubleSwag -> f "number" (Just "double")
      StringSwag -> f "string" Nothing
      ByteSwag -> f "string" (Just "byte")
      BinarySwag -> f "string" (Just "binary")
      BooleanSwag -> f "boolean" Nothing
      DateSwag -> f "string" (Just "date")
      DateTimeSwag -> f "string" (Just "date-time")
      PasswordSwag -> f "string" (Just "password")
      Model ModelSwag{..} -> 
         case modelSwagIsArray of
              True ->
                object [ "type" .= ("array" :: Text)
                       , "items" .= object [
                            "$ref" .= ("#/definitions/" <> unModelName modelSwagName)
                             ]
                          ]
              False ->
                object [
                    "$ref".= ("#/definitions/"<> unModelName modelSwagName)
                  ]

instance ToJSON ContentType where
  toJSON JSON        = String "application/json"
  toJSON XML         = String "application/xml"
  toJSON FormUrlEncoded = String "application/x-www-form-urlencoded"
  toJSON HTML        = String "text/html"
  toJSON PlainText   = String "text/plain; charset=utf-8"
  toJSON OctetStream = String "application/octet-stream"

instance ToJSON Scheme where
  toJSON Http  = String "http"
  toJSON Https = String "https"
  toJSON Ws    = String "ws"
  toJSON Wss   = String "wss"

instance ToJSON In where
  toJSON PathUrl  = "path"
  toJSON Query    = "query"
  toJSON Body     = "body"
  toJSON Header   = "header"
  toJSON FormData = "formData"

instance ToJSON PathName where
  toJSON (PathName x) = String (T.toLower x)

instance ToJSON SwaggerModel where
  toJSON SwaggerModel{..} =
    object $ [
        "type" .= ("object" :: Text)
      , "properties" .= H.fromList _swagProperties
      ] ++ maybeExample ++ maybeDescription
    where
      maybeDescription = maybe [] (\(Description x) -> [ "description" .= x ]) _swagDescription
      maybeExample = maybe [] (\x -> [ "example" .= x ]) _swagModelExample

setPath :: BasePath -> BasePath
setPath (BasePath "") = BasePath "/"
setPath (BasePath x) = BasePath x

instance ToJSON SwaggerAPI where
  toJSON SwaggerAPI{..} =
    object [
        "swagger"     .= ("2.0" :: Text)
      , "schemes"     .= _swaggerSchemes
      , "basePath"    .= _swaggerBasePath
      , "info"        .= _swaggerInfo
      , "paths"       .= Object (H.fromList $ map f $ H.toList _swaggerPaths)
      , "definitions" .= Object (H.fromList $ map g $ H.toList _swaggerDefinitions)
      , "tags" .= map (\(Tag tag) ->
                    object [ "name" .= tag
                           , "description" .= (T.toTitle tag <> " API")
                           ]) _swaggerTags
      ]
    where
      f (PathName pathName, sp) = (T.toLower pathName, toJSON sp)
      g (ModelName modelName, model) = (modelName, toJSON model)

instance ToJSON SwaggerPath where
  toJSON (SwaggerPath paths) = 
     Object . H.fromList . map f . H.toList $ paths
    where
      f (verb, sp) = (T.toLower $ toTxt verb, toJSON sp)

instance ToJSON Path where
  toJSON Path {..} =
    object [  "parameters" .= _params
            , "responses"  .= (Object . H.fromList . map f . H.toList $ _responses)
            , "produces"   .= _produces 
            , "consumes"   .= _consumes 
            , "summary"    .= _summary  
            , "tags" .= _tags
            ] 
    where f (Code x, resp) = (toTxt x, toJSON resp)
  
instance ToJSON Response where
  toJSON Response {..} = object $ [
      "description" .= _description
    , "headers" .= _responseHeaders
    ] ++ maybeModelName
    where
      maybeModelName =
        case _responseModelName of
          (ModelName "") -> []
          (ModelName name) ->
            case _responseIsArray of
              True -> [ "schema" .= object [
                           "type" .= ("array" :: Text)
                           , "items" .= object [
                              "$ref" .= ("#/definitions/" <> name) 
                       ]]]
              False -> ["schema".=object["$ref".=("#/definitions/"<> name)]]

instance ToJSON Param where
  toJSON Param{..} = 
    object $ [
        "in"          .= _in
      , "name"        .= _name
      , "description" .= _paramDescription
      , "required"    .= _required
      ]  ++ maybeType ++ maybeSchema
    where
      maybeSchema =
        case _in of
          Body -> [ "schema" .=
                      case _isArray of
                        False ->
                         object [ "$ref" .= ("#/definitions/" <> _name) ]
                        True ->
                          object [
                              "type" .= ("array" :: Text)
                             , "items" .= object [
                                 "$ref" .= ("#/definitions/" <> _name )
                               ]
                             ]
                         ]
          _ -> []
      maybeType =
        case _type of
          Nothing -> []
          Just pType -> [ "type" .= pType ]

instance ToSwaggerModel a => ToSwaggerModel (Maybe a) where
  toSwagModel _ = toSwagModel (Proxy :: Proxy a)

instance ToJSON Info where
  toJSON Info{..} =
    object $ [
        "title"   .= _swaggerInfoTitle
      , "version" .= _swaggerVersion
      , "description" .= _swaggerAPIDescription
      ] ++ (maybe [] (pure .  ("license" .=)) _license)

toTxt :: Show a => a -> Text
toTxt = T.pack . show

-- Add Path Summaries, and tags

