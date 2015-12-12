{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DefaultSignatures          #-}
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
{-# LANGUAGE ConstraintKinds            #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances       #-}
#endif
------------------------------------------------------------------------------
module Servant.Swagger.Internal  where
------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.Types ( typeMismatch )
import qualified Data.Set as S
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
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           GHC.TypeLits
import           Servant.API hiding (Header)
import qualified Servant.API.Header as H
import qualified Data.UUID as UUID

-- | Helper to generate swagger.json file
createSwaggerJson :: SwaggerAPI -> IO ()
createSwaggerJson = BL8.writeFile "swagger.json" . encode

-- | This is the root document object for the API specification.
data SwaggerAPI = SwaggerAPI {
     _swaggerInfo               :: Info -- ^ Required, provides metadata about the API
  ,  _swaggerPaths              :: HM.HashMap PathName SwaggerOperation -- ^ Required
  ,  _swaggerSchemes            :: Maybe [Scheme] -- ^ Schemes for this API (i.e HTTP/HTTPS)
  ,  _swaggerDefinitions        :: HM.HashMap ModelName SwaggerModel -- ^
  ,  _swaggerTags               :: Maybe [Tag] -- ^ A list of tags that provide additional metadat
  ,  _swaggerBasePath           :: Maybe BasePath -- ^ The base path upon which this API is served
  ,  _swaggerHostName           :: Maybe HostName -- ^ Host name or IP
  ,  _swaggerSecurityDefintions :: Maybe [SecurityDefinition]
     -- ^ Security scheme definitions that can be used across the specification.
  ,  _swaggerExternalDocs       :: Maybe ExternalDocs
  } deriving Show

-- | Smart Constructor for `SwaggerAPI`
defSwaggerAPI :: Info -> SwaggerAPI
defSwaggerAPI info = SwaggerAPI info mempty mempty mempty mempty mempty mempty mempty mempty

-- | `ToJSON` for `SwaggerAPI`
instance ToJSON SwaggerAPI where
  toJSON SwaggerAPI{..} =
    object $ [
        "swagger"     .= ("2.0" :: Text)
      , "info"        .= _swaggerInfo
      , "paths"       .= do Object $ HM.fromList $ map f $ HM.toList _swaggerPaths
      , "definitions" .= do Object $ HM.fromList $ map g $ HM.toList _swaggerDefinitions
      ] ++
      [ "host" .= _swaggerHostName | isJust _swaggerHostName ] ++
      [ "schemes" .= _swaggerSchemes | isJust _swaggerSchemes ] ++
      [ "basePath" .= _swaggerBasePath | isJust _swaggerBasePath ] ++
      [ "externalDocs" .= _swaggerExternalDocs | isJust _swaggerExternalDocs ] ++
      [ "tags" .=  _swaggerTags | isJust _swaggerTags ]
    where
      f (PathName pathName, sp) = (T.toLower pathName, toJSON sp)
      g (ModelName modelName, model) = (modelName, toJSON model)

data ExternalDocs = ExternalDocs {
    _externalDescription :: Maybe ExternalDescription -- ^ Optional Description
  , _externalURL :: ExternalURL -- ^ Required URL that links supporting documentation
  } deriving (Show, Eq)

instance ToJSON ExternalDocs where
  toJSON ExternalDocs{..} =
    object $ [ "url" .= _externalURL ] ++
             [ "description" .=  _externalDescription
             | isJust _externalDescription
             ]

instance Monoid ExternalDocs where
  mempty = ExternalDocs mempty mempty
  (ExternalDocs a1 b1) `mappend` (ExternalDocs a2 b2)
    = ExternalDocs (a1 <> a2) (b1 <> b2)

-- | External Description for External Documentation API
newtype ExternalDescription = ExternalDescription Text
  deriving (Show, Eq, Monoid, ToJSON)

-- | External Description for External Documentation API
newtype ExternalURL = ExternalURL Text
  deriving (Show, Eq, Monoid, ToJSON)

-- | Type used to accumulate information of a Servant path
data SwaggerRoute = SwaggerRoute {
    _routePathName    :: PathName -- ^ Accumulated `PathName`
  , _routeConsumes    :: [ContentType]  -- ^ Content Types a route consumes
  , _routeModels      :: HM.HashMap ModelName SwaggerModel  -- ^ Models present in route
  , _routeParams      :: [Param]  -- ^ Params present in route
  , _routeVerb        :: Verb  -- ^ Verb of route
  , _routePathSummary :: PathSummary -- ^ Summary of path
  , _routeResponses   :: HM.HashMap Text Response -- ^ Additional responses for a Route
  , _routeTags        :: [Tag]  -- ^ Tags present for this Route
  } deriving Show

-- | Default Route used to build up
defSwaggerRoute :: SwaggerRoute
defSwaggerRoute = SwaggerRoute mempty [] [] [] Get mempty [] []

-- | Default
-- <http://swagger.io/specification/#contactObject contact>
defSwaggerInfo :: Info
defSwaggerInfo =
  Info (APITitle mempty)
    (APIVersion "2.0") (APIDescription mempty) Nothing Nothing Nothing

-- | Contact name of `Contact` object
newtype ContactName = ContactName Text
  deriving (Show, Eq, ToJSON, FromJSON, Ord, Monoid)

-- | Contact URL of `Contact` object
newtype ContactURL = ContactURL Text
  deriving (Show, Eq, ToJSON, FromJSON, Ord, Monoid)

-- | Contact Email of `Contact` object
newtype ContactEmail = ContactEmail Text
  deriving (Show, Eq, ToJSON, FromJSON, Ord, Monoid)

-- | Contact Object
data Contact = Contact {
    _contactName :: ContactName
  , _contactURL :: ContactURL
  , _contactEmail :: ContactEmail
  } deriving (Show, Eq, Ord)

instance Monoid Contact where
  mempty = Contact mempty mempty mempty
  (Contact a1 b1 c1) `mappend` (Contact a2 b2 c2) =
     Contact (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- | Contact Object
instance ToJSON Contact where
  toJSON Contact{..} =
    object [
        "name"  .= _contactName
      , "url"   .= _contactURL
      , "email" .= _contactEmail
      ]

-- | Description of API
newtype APIDescription = APIDescription { _unApiDesc :: Text }
   deriving (Show, Eq, ToJSON, Monoid)

-- | Terms of Service of API located in `Info`
newtype TermsOfService = TermsOfService Text
   deriving (Show, Eq, ToJSON, Monoid)

-- | A Swagger metadata for a Servant header
data SwaggerHeader = SwaggerHeader {
    headerDescription :: Text -- ^ Header description
  , headerType :: SwaggerParamType  -- ^ Type of Header
  , headerName :: Text   -- ^ Name of Header
  } deriving (Show, Eq)

-- | A container for the expected responses of an operation.
data Response = Response {
    _responseDescription :: Text -- ^ Description of Response
  , _responseModelName :: ModelName  -- ^ `Model` this Response returns
  , _responseHeaders :: HM.HashMap Text SwaggerHeader  -- ^ HashMap of headers
  , _responseIsArray :: Bool -- ^ Does the response return an Array?
  , _responseCode :: Code -- ^ Response code this route returns
  } deriving (Show, Eq)

-- | Default Response for a Path
defResponse :: Response
defResponse = Response mempty (ModelName mempty) mempty False (Code 200)

-- | Name of `Tag`, that can be applied to an operation
newtype TagName = TagName Text deriving (Show, Eq, Ord, ToJSON, FromJSON)

-- | Description of `Tag`
newtype TagDescription = TagDescription Text deriving (Show, Eq, Ord, ToJSON, FromJSON)

-- | Allows adding meta data to a single tag that is used by the Operation Object
data Tag = Tag {
    _tagName        :: TagName -- ^ Name of `Tag`
  , _tagDescription :: TagDescription  -- ^ Description of `Tag`
  } deriving (Show, Eq, Ord)

-- | `ToJSON` `Tag` instance
instance ToJSON Tag where
  toJSON Tag{..} = object [ "name" .= _tagName, "description" .= _tagDescription ]

-- | `FromJSON` `Tag` instance
instance FromJSON Tag where
  parseJSON (Object o) = Tag <$> o .: "name" <*> o .:  "description"
  parseJSON x = typeMismatch "Tag" x

-- | A declaration of the security schemes available to be used in the specification
data SecurityDefinition =
    OAuthDef OAuth -- ^ OAuth
  | APIKeyDef APIKey -- ^ APIKey
  | BasicAuthDef BasicAuth  -- ^ BasicAuth
  deriving Show

-- | Basic Authentication
data BasicAuth = BasicAuth deriving Show

-- ^ OAuth Flow
data OAuthFlow = Implicit | Password | Application | AccessCode deriving (Show)
-- ^ OAuth URL
data OAuthURL = OAuthURL deriving (Show)
data TokenURL = TokenURL deriving (Show)
data Scopes = Scopes deriving (Show)

-- | OAuth Authentication
data OAuth = OAuth {
    oauthDescription :: Maybe Description -- ^ Optional description for Swagger OAuth object
  , oauthFlow        :: OAuthFlow         -- ^ OAuth Flow
  , oauthURL         :: OAuthURL          -- ^ OAuth URL
  , oauthTokenURL    :: TokenURL          -- ^ Token URL
  , scopes           :: Scopes            -- ^ OAuth Scopes
  } deriving Show

-- | API Key
newtype APIKeyName =
  APIKeyName Text deriving (Show, Eq)

-- | API Location (i.e. is it located in a Query Param or a Header)
data APIKeyIn = APIKeyQueryParam
              | APIKeyHeader deriving (Show, Eq)

-- | API Key Object
data APIKey = APIKey {
    apiKeyDescription :: Maybe Description -- ^ Optional `Description`
  , apiKeyName        :: APIKeyName  -- ^ Optional `Description`
  , apiKeyIn          :: APIKeyIn
  } deriving Show

-- | API Key `ToJSON`
instance ToJSON APIKey where
  toJSON APIKey{..} =
    object [ "api_key" .= ([] :: [Int]) ]

-- | HostName
newtype HostName = HostName Text
  deriving (Show, Eq, IsString, ToJSON, FromJSON, Monoid)

-- | BasePath
newtype BasePath = BasePath Text
   deriving (Show, Eq, ToJSON, FromJSON, Monoid)

-- | Info Objet
data Info = Info {
    _swaggerInfoTitle      :: APITitle -- ^ API Title
  , _swaggerVersion        :: APIVersion -- ^ API Version
  , _swaggerAPIDescription :: APIDescription -- ^ API Description
  , _license               :: Maybe APILicense -- ^ API Description
  , _contact               :: Maybe Contact
  , _termsOfService        :: Maybe TermsOfService
  } deriving (Show, Eq)

instance Monoid Info where
  mempty = Info mempty mempty mempty mempty mempty mempty
  (Info a1 b1 c1 d1 e1 f1) `mappend` (Info a2 b2 c2 d2 e2 f2)
    = Info (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)

data APILicense = APILicense {
     _licenseName :: Text
  ,  _licenseUrl  :: Maybe Text
  } deriving (Show, Eq)

instance Monoid APILicense where
  mempty = APILicense mempty mempty
  (APILicense a1 b1) `mappend` (APILicense a2 b2) = APILicense (a1 <> a2) (b1 <> b2)

data SwaggerOperation = SwaggerOperation {
     _paths :: HM.HashMap Verb Operation
  } deriving Show

data SwagResult = SwagResult {
    _resultPaths  :: HM.HashMap PathName SwaggerOperation
  , _resultModels :: HM.HashMap ModelName SwaggerModel
  } deriving (Show)

data Verb = Post | Get | Put | Options | Head | Delete | Patch
  deriving (Show, Eq, Read, Generic)

newtype PathSummary = PathSummary Text
    deriving (Show, Eq, ToJSON, FromJSON, Monoid, IsString)

data Operation = Operation {
     _summary   :: PathSummary
   , _params    :: [Param]
   , _responses :: HM.HashMap Code Response
   , _produces  :: [ContentType]
   , _consumes  :: [ContentType]
   , _tags      :: [Tag]
   , _operationId :: Maybe OperationId
   , _description :: PathDescription
   , _deprecated :: Maybe Deprecated
  } deriving Show

instance Monoid Operation where
  mempty = Operation mempty mempty mempty mempty mempty mempty mempty mempty mempty
  (Operation a1 b1 c1 d1 e1 f1 g1 h1 i1) `mappend`
    (Operation a2 b2 c2 d2 e2 f2 g2 h2 i2) =
       Operation (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)
          (h1 <> h2) (i1 <> i2)

newtype Deprecated = Deprecated Bool deriving (Show, Eq, ToJSON)

instance Monoid Deprecated where
  mempty = Deprecated False
  (Deprecated False) `mappend` (Deprecated False) = Deprecated False
  _ `mappend` _ = Deprecated True

newtype OperationId = OperationId Text deriving (Show, Eq, ToJSON, Monoid)
newtype PathDescription = PathDescription Text deriving (Show, Eq, ToJSON, Monoid)

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

newtype APIVersion = APIVersion Text deriving (Show, Eq, ToJSON, Monoid)
newtype APITitle = APITitle Text deriving (Show, Eq, ToJSON, Monoid)
newtype PathName = PathName { unPathName :: Text }
  deriving (Show, Eq, Hashable, Monoid)

newtype ModelName = ModelName { unModelName :: Text }
   deriving (Show, Eq, Hashable, Monoid)

newtype Description =
  Description { unDescription :: Text } deriving (Show, Eq, ToJSON, Monoid)

data SwaggerModel = SwaggerModel {
     _swagModelName    :: ModelName
   , _swagProperties   :: [(Text, SwaggerType)]
   , _swagDescription  :: Maybe Description
   , _swagModelExample :: Maybe Value
   , _swagModelRequired :: [Text]
   } deriving (Show, Eq)

emptyModel :: SwaggerModel
emptyModel = SwaggerModel (ModelName mempty) mempty mempty Nothing mempty

data SwaggerRouteDescription = SwaggerRouteDescription {
    _swagRouteTags      :: [Tag]       -- ^ Tags
  , _swagRouteSummary   :: PathSummary -- ^ Description of this endpoint
  , _swagRouteResponses :: HM.HashMap Code Response  -- ^ Additional responses for this endpoint
  , _swagRouteModels    :: HM.HashMap ModelName SwaggerModel
  , _swagRouteOperationId :: Maybe OperationId
  , _swagRouteDescription :: PathDescription
  } deriving Show

emptyRouteDescription :: SwaggerRouteDescription
emptyRouteDescription = SwaggerRouteDescription mempty mempty mempty mempty mempty mempty

$(makeLenses ''SwaggerModel)
$(makeLenses ''ExternalDocs)
$(makeLenses ''SwaggerRouteDescription)
$(makeLenses ''SwagResult)
$(makeLenses ''SwaggerRoute)
$(makeLenses ''SwaggerAPI)
$(makeLenses ''Info)
$(makeLenses ''Contact)
$(makeLenses ''APILicense)
$(makeLenses ''Operation)
$(makeLenses ''Tag)
$(makeLenses ''Response)

defExternalDocs :: ExternalURL -> ExternalDocs
defExternalDocs url = mempty & externalURL .~ url

------------------------------------------------------------------------------
-- | Swaggin'
class HasSwagger h where
  toSwaggerDocs :: Proxy h -> SwaggerRoute -> SwagResult
------------------------------------------------------------------------------
class ToSwaggerDescription a where toSwaggerDescription :: Proxy a -> Text
class ToHeader a where toHeader :: Proxy a -> SwaggerHeader
class ToResponseHeaders as where toResponseHeaders :: Proxy as -> HM.HashMap Text SwaggerHeader
instance ToResponseHeaders '[] where toResponseHeaders Proxy = []

instance (ToHeader x, ToResponseHeaders xs) => ToResponseHeaders (x ': xs)  where
  toResponseHeaders Proxy = HM.union header' (toResponseHeaders (Proxy :: Proxy xs))
    where
      header' = transHeader (toHeader (Proxy :: Proxy x))

transHeader :: SwaggerHeader -> HM.HashMap Text SwaggerHeader
transHeader r@SwaggerHeader{..} = HM.fromList [(headerName, r)]

------------------------------------------------------------------------------
instance (HasSwagger rest, KnownSymbol sym) => HasSwagger (sym :> rest) where
  toSwaggerDocs Proxy swagRoute =
     toSwaggerDocs (Proxy :: Proxy rest) $ swagRoute & routePathName %~ flip (<>) path
   where path = PathName $ "/" <> T.pack (symbolVal (Proxy :: Proxy sym))

instance (HasSwagger left, HasSwagger right) => HasSwagger (left :<|> right) where
  toSwaggerDocs Proxy swagRoute =
    let swagLeft = toSwaggerDocs (Proxy :: Proxy left) swagRoute
        swagRight = toSwaggerDocs (Proxy :: Proxy right) swagRoute
        paths  = HM.unionWith f (swagLeft ^. resultPaths) (swagRight ^. resultPaths)
        models = HM.union (swagLeft ^. resultModels) (swagRight ^. resultModels)
    in SwagResult paths models
      where f (SwaggerOperation l) (SwaggerOperation r) = SwaggerOperation (HM.union l r)

class ToSwaggerParamType a where toSwaggerParamType :: Proxy a -> SwaggerParamType
instance ToSwaggerParamType Int where toSwaggerParamType = const IntegerSwagParam
instance ToSwaggerParamType Integer where toSwaggerParamType = const IntegerSwagParam
instance ToSwaggerParamType UUID.UUID where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType String where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Text where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType L.Text where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType BL8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType B8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Double where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Float where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Bool where toSwaggerParamType Proxy = BooleanSwagParam

instance
  ToSwaggerParamType a => ToSwaggerParamType [a] where
    toSwaggerParamType _ = ArraySwagParam

class ToHeaderDescription a where
  toHeaderDescription :: Proxy a -> Text

instance ( ToSwaggerParamType headerType
         , KnownSymbol headerName
         , ToHeaderDescription headerName
         ) => ToHeader (H.Header headerName headerType) where
  toHeader Proxy = SwaggerHeader desc ht hn
      where
        desc = T.pack . symbolVal $ (Proxy :: Proxy headerName)
        hn = T.pack . symbolVal $ (Proxy :: Proxy headerName)
        ht = toSwaggerParamType (Proxy :: Proxy headerType)

class SwaggerAccept a where toSwaggerAccept :: Proxy a -> ContentType
instance SwaggerAccept JSON where toSwaggerAccept Proxy = JSON
instance SwaggerAccept HTML where toSwaggerAccept Proxy = HTML
instance SwaggerAccept XML where toSwaggerAccept Proxy = XML
instance SwaggerAccept FormUrlEncoded where toSwaggerAccept Proxy = FormUrlEncoded
instance SwaggerAccept PlainText where toSwaggerAccept Proxy = PlainText
instance SwaggerAccept OctetStream where toSwaggerAccept Proxy = OctetStream
------------------------------------------------------------------------------
class SwaggerAcceptTypes (xs :: [*]) where toSwaggerAcceptTypes :: Proxy xs -> [ContentType]
instance SwaggerAcceptTypes '[] where toSwaggerAcceptTypes Proxy = []
instance (SwaggerAccept x, SwaggerAcceptTypes xs) => SwaggerAcceptTypes (x ': xs) where
  toSwaggerAcceptTypes Proxy =
    toSwaggerAccept (Proxy :: Proxy x) : toSwaggerAcceptTypes (Proxy :: Proxy xs)
------------------------------------------------------------------------------
class ToVerb a where toVerb :: Proxy a -> Verb
instance ToVerb Get where toVerb Proxy   = Get
instance ToVerb Put where toVerb Proxy   = Put
instance ToVerb Patch where toVerb Proxy = Patch
instance ToVerb Post where toVerb Proxy  = Post
instance ToVerb Delete where toVerb Proxy  = Delete
instance ToVerb Options where toVerb Proxy  = Options

class ToSwaggerModel a where
  toSwagModel  :: Proxy a -> SwaggerModel
  toSwagModelName :: Proxy a -> ModelName
  toSwagModelName = _swagModelName . toSwagModel
  default toSwagModel :: (Generic a, GToSwaggerModel (Rep a)) => Proxy a -> SwaggerModel
  toSwagModel = undefined

class GToSwaggerModel a where
  gToSwaggerModel :: Proxy a -> f a -> SwaggerModel

instance ToSwaggerModel () where
  toSwagModel Proxy = emptyModel

instance ToSwaggerModel SwaggerAPI where
  toSwagModel Proxy = emptyModel

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs)
      => HasSwagger (verb xs returnType) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerOperation [(toVerb (Proxy :: Proxy verb), path)]
        path = mempty & summary .~ swagRoute ^. routePathSummary
                      & params .~ swagRoute ^. routeParams
                      & responses .~ [(_responseCode response, response)]
                      & produces .~ toSwaggerAcceptTypes (Proxy :: Proxy xs)
                      & consumes .~ swagRoute ^. routeConsumes
    in SwagResult [(pathName, swagPath)] newModels
      where
        response = Response "OK" (swagModel ^. swagModelName) [] False 200
        pathName | swagRoute ^. routePathName == PathName "" = PathName "/"
                 | otherwise = swagRoute ^. routePathName
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = bool (swagRoute ^. routeModels)
                         (HM.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptyModel)

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs)
      => HasSwagger (verb xs [returnType]) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerOperation [(toVerb (Proxy :: Proxy verb), path)]
        path = mempty & summary .~ swagRoute ^. routePathSummary
                      & params .~ swagRoute ^. routeParams
                      & responses .~ [(_responseCode response, response)]
                      & produces .~ toSwaggerAcceptTypes (Proxy :: Proxy xs)
                      & consumes .~ swagRoute ^. routeConsumes
    in SwagResult [(pathName, swagPath)] newModels
      where
        response = Response "OK" (swagModel ^. swagModelName) [] False 200
        pathName | swagRoute ^. routePathName == PathName "" = PathName "/"
                 | otherwise = swagRoute ^. routePathName
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = bool (swagRoute ^. routeModels)
                         (HM.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptyModel)

instance
#if MIN_VERSION_base(4,8,0)
   {-# OVERLAPPING #-}
#endif
 (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs, ToResponseHeaders ls)
    => HasSwagger (verb xs (Headers ls [returnType])) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerOperation [(toVerb (Proxy :: Proxy verb), path)]
        path = mempty & summary .~ swagRoute ^. routePathSummary
                      & params .~ swagRoute ^. routeParams
                      & responses .~ [(_responseCode response, response)]
                      & produces .~ toSwaggerAcceptTypes (Proxy :: Proxy xs)
                      & consumes .~ swagRoute ^. routeConsumes
    in SwagResult [(swagRoute ^. routePathName, swagPath)] newModels
      where
        response = Response "OK" (swagModel ^. swagModelName)
                     (toResponseHeaders (Proxy :: Proxy ls)) True 200
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = bool (swagRoute ^. routeModels)
                         (HM.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptyModel)

instance
#if MIN_VERSION_base(4,8,0)
   {-# OVERLAPPING #-}
#endif
 (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs, ToResponseHeaders ls)
    => HasSwagger (verb xs (Headers ls returnType)) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerOperation [(toVerb (Proxy :: Proxy verb), path)]
        path = mempty & summary .~ swagRoute ^. routePathSummary
                      & params .~ swagRoute ^. routeParams
                      & responses .~ [(_responseCode response, response)]
                      & produces .~ toSwaggerAcceptTypes (Proxy :: Proxy xs)
                      & consumes .~ swagRoute ^. routeConsumes
    in SwagResult [(swagRoute ^. routePathName, swagPath)] newModels
      where
        response = Response "OK" (swagModel ^. swagModelName) rspHeaders False 200
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        rspHeaders = (toResponseHeaders (Proxy :: Proxy ls))
        newModels = bool (swagRoute ^. routeModels)
                         (HM.insert _swagModelName swagModel (swagRoute ^. routeModels))
                         (swagModel /= emptyModel)

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (Capture sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newPath = PathName $ mconcat ["/{",pName,"}"]
        newParam = Param PathUrl pName
                     (Just $ toSwaggerParamType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True True Nothing False
        newSwaggerRoute = swagRoute & routePathName %~ flip (<>) newPath
                                    & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParam sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just $ toSwaggerParamType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing False
        newSwaggerRoute = swagRoute & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParams sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
        ptyp = toSwaggerParamType (Proxy :: Proxy typ)
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just ArraySwagParam) (Just $ ItemObject ptyp)
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing True
        newSwaggerRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Query Flag
instance (ToSwaggerDescription sym, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryFlag sym :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just StringSwagParam) Nothing
                       (toSwaggerDescription (Proxy :: Proxy sym)) True False Nothing False
        newSwaggerRoute = swagRoute & routeParams %~ (:) newParam

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
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
        newSwaggerRoute = swagRoute & routeParams %~ (:) newParams
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
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
         swagModel@SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwaggerRoute =
           swagRoute & routeModels %~ model
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         model | swagModel == emptyModel = (<> mempty)
               | otherwise = HM.insert _swagModelName (toSwagModel (Proxy :: Proxy model))
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
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwaggerRoute
      where
         swagModel@SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwaggerRoute =
           swagRoute & routeModels %~ model
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         model | swagModel == emptyModel = (<> mempty)
               | otherwise = HM.insert _swagModelName (toSwagModel (Proxy :: Proxy model))
         newParam =
            case _swagModelName of
              (ModelName "") -> []
              name -> [ Param Body (unModelName name) Nothing Nothing
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing True]

class ToModelExample model where toExample :: Proxy model -> Maybe Value

instance ToJSON SwaggerHeader where
  toJSON SwaggerHeader{..} =
    object [
         "type" .= headerType
      ,  "description" .= headerDescription
      ]

instance Monoid SwaggerOperation where
  mempty = SwaggerOperation HM.empty
  SwaggerOperation a `mappend` SwaggerOperation b =
    SwaggerOperation ( a <> b )

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
  toJSON ArraySwagParam   = String "array"
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
      , "properties" .= HM.fromList _swagProperties
      ] ++ maybeExample ++ maybeDescription ++ requiredList
    where
      requiredList = [ "required" .= _swagModelRequired | not (null _swagModelRequired) ]
      maybeDescription = maybe [] (\(Description x) -> [ "description" .= x ]) _swagDescription
      maybeExample = maybe [] (\x -> [ "example" .= x ]) _swagModelExample

setPath :: BasePath -> BasePath
setPath (BasePath "") = BasePath "/"
setPath (BasePath x) = BasePath x


instance ToJSON SwaggerOperation where
  toJSON (SwaggerOperation paths) =
     Object . HM.fromList . map f . HM.toList $ paths
    where
      f (verb, sp) = (T.toLower $ toTxt verb, toJSON sp)

instance ToJSON Operation where
  toJSON Operation {..} =
    object $ [ "parameters"  .= _params
             , "responses"   .= do Object . HM.fromList . map f . HM.toList $ _responses
             , "produces"    .= _produces
             , "consumes"    .= _consumes
             , "summary"     .= _summary
             , "tags"        .=  map _tagName _tags
             , "description" .= _description
             ] ++ [ "deprecated" .= _deprecated | isJust _deprecated ]
               ++ [ "operationId" .= _operationId | isJust _operationId ]
    where f (Code x, resp) = (toTxt x, toJSON resp)

instance ToJSON Response where
  toJSON Response {..} = object $ [
      "description" .= _responseDescription
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
      ]  ++ maybeSchema ++ [ "type" .= _type | isJust _type ]
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

instance ToSwaggerModel a => ToSwaggerModel (Maybe a) where
  toSwagModel _ = toSwagModel (Proxy :: Proxy a)

instance ToJSON Info where
  toJSON Info{..} =
    object $ [
        "title"   .= _swaggerInfoTitle
      , "version" .= _swaggerVersion
      , "description" .= _swaggerAPIDescription
      ] ++ [ "license" .=  _license | isJust _license ]
        ++ [ "contact" .=  _contact | isJust _contact ]
        ++ [ "termsOfService" .=  _termsOfService | isJust _termsOfService ]

toTxt :: Show a => a -> Text
toTxt = T.pack . show

newtype SwaggerRouteInfo a = SwaggerRouteInfo SwagResult -- deriving Monoid

instance Monoid (SwaggerRouteInfo a) where
  mempty = SwaggerRouteInfo mempty
  SwaggerRouteInfo s1 `mappend` SwaggerRouteInfo s2
    = SwaggerRouteInfo (s1 `mappend` s2)

instance Monoid SwagResult where
  mempty = SwagResult mempty mempty
  SwagResult x1 y1 `mappend` SwagResult x2 y2
    = SwagResult (HM.unionWith mergePaths x1 x2) (HM.union y1 y2)
      where
        mergePaths (SwaggerOperation l) (SwaggerOperation r) = SwaggerOperation (HM.unionWith g l r)
        g p1 p2 =
          p1 & summary %~ (<>) (p2 ^. summary)
             & responses %~ HM.union (p2 ^. responses)
             & tags %~ (++) (p2 ^. tags)
             & operationId .~ p1 ^. operationId <> p2 ^. operationId
             & description .~ p1 ^. description <> p2 ^. description

swaggerPathInfo
  :: ( IsElem endpoint layout, HasLink endpoint, HasSwagger endpoint, HasSwagger layout )
  => Proxy endpoint
  -> Proxy layout
  -> SwaggerRouteDescription
  -> SwaggerRouteInfo layout
swaggerPathInfo pEndpoint pLayout SwaggerRouteDescription{..} = swagResult
  where
    f [(pName, SwaggerOperation swagPath)] =
        [(pName, SwaggerOperation $ HM.fromList . g . HM.toList $ swagPath)]
    f _ = error "Route non-existant, impossible"
    g [(verb, path)] = [(verb, newPath path)]
    g _ = error "Route non-existant, impossible"
    newPath p = p & summary .~ _swagRouteSummary
                  & operationId .~  _swagRouteOperationId
                  & description .~  _swagRouteDescription
                  & responses %~ HM.union _swagRouteResponses
                  & tags %~ (++) _swagRouteTags
    swagResult =
      let finalDocs = toSwaggerDocs pLayout defSwaggerRoute
          SwagResult paths models = toSwaggerDocs pEndpoint defSwaggerRoute
          newModels = _swagRouteModels `HM.union` models
          newPaths = HM.fromList . f . HM.toList $ paths
          pathDocs = SwagResult newPaths newModels
      in SwaggerRouteInfo (finalDocs <> pathDocs)

getAllTags :: SwagResult -> [Tag]
getAllTags (SwagResult paths _) =
    S.toList . S.fromList . _tags =<< HM.elems =<< _paths <$> HM.elems paths
