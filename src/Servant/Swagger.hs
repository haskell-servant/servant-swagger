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
------------------------------------------------------------------------------
module Servant.Swagger  where
------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Data.Bool
import           Data.Data
import           Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson
import           GHC.Generics
import           Data.List
import           Data.Maybe
import           Data.String
import           Control.Lens
import           Data.Char
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           GHC.Exts (Constraint)
import           GHC.TypeLits
import           Servant.API 
import           Servant.API.ResponseHeaders
import           Servant.Swagger.Internal
import qualified Servant.API.Header as H
#if !MIN_VERSION_base(4,8,0)
  {-# LANGUAGE OverlappingInstances   #-}
#endif

------------------------------------------------------------------------------
-- | Swaggin'
class HasSwagger h where
  toSwaggerDocs :: Proxy h -> SwagRoute -> SwaggerAPI
------------------------------------------------------------------------------
class ToSwaggerDescription a where toSwaggerDescription :: Proxy a -> Text
class ToResponseHeader a where toResponseHeader :: Proxy a -> ResponseHeader
class ToResponseHeaders as where toResponseHeaders :: Proxy as -> [ ResponseHeader ]
instance ToResponseHeaders '[] where toResponseHeaders Proxy = []
instance (ToResponseHeader x, ToResponseHeaders xs) => ToResponseHeaders (x ': xs) where
   toResponseHeaders Proxy =
     toResponseHeader (Proxy :: Proxy x) : toResponseHeaders (Proxy :: Proxy xs)
------------------------------------------------------------------------------
instance (HasSwagger rest, KnownSymbol sym) => HasSwagger (sym :> rest) where
  toSwaggerDocs Proxy swagRoute =
    toSwaggerDocs (Proxy :: Proxy rest) (swagRoute & routePathName %~ (<> path))
    where path = PathName $ "/" <> T.pack (symbolVal (Proxy :: Proxy sym))

instance ( ToSwaggerParamType headerType
         , KnownSymbol headerName
         ) => ToResponseHeader (H.Header headerName headerType) where
  toResponseHeader Proxy = ResponseHeader mempty ht hn
      where
        hn = T.pack . symbolVal $ (Proxy :: Proxy headerName)
        ht = toSwaggerParamType (Proxy :: Proxy headerType)
------------------------------------------------------------------------------
instance ToResponseHeaders ls => ToResponseHeaders (Headers ls a) where
  toResponseHeaders Proxy = toResponseHeaders (Proxy :: Proxy ls)
------------------------------------------------------------------------------
instance (HasSwagger left, HasSwagger right) => HasSwagger (left :<|> right) where
  toSwaggerDocs Proxy swagRoute@(SwagRoute _ _ _ _ _ _ _ info schemes) =
    let swagLeft = toSwaggerDocs (Proxy :: Proxy left) swagRoute
        swagRight = toSwaggerDocs (Proxy :: Proxy right) swagRoute
        paths  = H.unionWith f (swagLeft ^. swaggerPaths) (swagRight ^. swaggerPaths)
        models = H.union (swagLeft ^. swaggerDefinitions) (swagRight ^. swaggerDefinitions)
    in SwaggerAPI info schemes paths models (nub $ swagLeft ^. swaggerTags ++ swagRight ^. swaggerTags)
      where f (SwaggerPath l) (SwaggerPath r) = SwaggerPath (H.union l r)
------------------------------------------------------------------------------
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
  toSwaggerAcceptTypes Proxy = toSwaggerAccept (Proxy :: Proxy x) : toSwaggerAcceptTypes (Proxy :: Proxy xs)
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
------------------------------------------------------------------------------
getTag :: PathName -> [Tag]
getTag (PathName path) = do
  let xs = T.takeWhile (/='/') (T.drop 1 path)
  bool [Tag xs] [] $ T.any (=='{') xs

class ToSwaggerPathSummary a where
  toSwaggerPathSummary :: Proxy a -> PathSummary

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs) => HasSwagger (verb xs returnType) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path (swagRoute ^. routePathSummary) (swagRoute ^. routeParams)
            [(200, Response "Success" (swagModel ^. swagModelName) (swagRoute ^. routeRespHeaders) False)]
               (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes)
                   (getTag $ swagRoute ^. routePathName)
    in SwaggerAPI (swagRoute ^. routeSwagInfo) (swagRoute ^. routeSwagSchemes) 
      [(swagRoute ^. routePathName, swagPath)] newModels (getTag $ swagRoute ^. routePathName)
      where
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = case _swagModelName of
                      Nothing -> (swagRoute ^. routeModels)
                      Just name -> H.insert name  swagModel (swagRoute ^. routeModels)
instance
#if MIN_VERSION_base(4,8,0)
    {-# OVERLAPPING #-}
#endif
 (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs) => HasSwagger (verb xs [returnType]) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path (swagRoute ^. routePathSummary) (swagRoute ^. routeParams)
            [(200, Response "Success" (swagModel ^. swagModelName) (swagRoute ^. routeRespHeaders) True)]
               (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes)
                   (getTag $ swagRoute ^. routePathName)
    in SwaggerAPI (swagRoute ^. routeSwagInfo) (swagRoute ^. routeSwagSchemes) 
      [(swagRoute ^. routePathName, swagPath)] newModels (getTag $ swagRoute ^. routePathName)
      where
        swagModel@SwaggerModel{..} = toSwagModel (Proxy :: Proxy returnType)
        newModels = case _swagModelName of
                      Nothing -> (swagRoute ^. routeModels)
                      Just name -> H.insert name  swagModel (swagRoute ^. routeModels)

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
    SwaggerAPI (swagRoute ^. routeSwagInfo) (swagRoute ^. routeSwagSchemes) 
      [(swagRoute ^. routePathName, mempty )] [] (getTag $ swagRoute ^. routePathName)

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
  HasSwagger (Header sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        newSwagRoute = swagRoute & routeParams %~ (:) newParams
        pName = T.pack $ symbolVal (Proxy :: Proxy sym) 
        pDesc = toSwaggerDescription (Proxy :: Proxy typ) 
        typ = toSwaggerParamType (Proxy :: Proxy typ) 
        newParams = Param Servant.Swagger.Internal.Header pName (Just typ)
                       Nothing pDesc False True Nothing False

------------------------------------------------------------------------------
-- | ReqBody Object
instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
  (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
  HasSwagger (ReqBody ctypes model :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
         SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwagRoute =
           swagRoute & routeModels %~ (maybe (<> mempty)
               (\name -> H.insert name (toSwagModel (Proxy :: Proxy model))) _swagModelName)
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         newParam =
            case _swagModelName of
              Nothing -> []
              Just name -> [ Param Body (unModelName name) Nothing Nothing
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing False]

------------------------------------------------------------------------------
-- | ReqBody Array
instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPABLe #-}
#endif
  (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
  HasSwagger (ReqBody ctypes [model] :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
         SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         newSwagRoute =
           swagRoute & routeModels %~ (maybe (<> mempty)
               (\name -> H.insert name (toSwagModel (Proxy :: Proxy model))) _swagModelName)
                     & routeParams %~ (++) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         newParam = 
            case _swagModelName of
              Nothing -> []
              Just name -> [ Param Body (unModelName name) Nothing Nothing
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing True]

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

class ToSwaggerModel a where toSwagModel  :: Proxy a -> SwaggerModel

instance ToSwaggerModel () where toSwagModel Proxy = SwaggerModel Nothing [] Nothing Nothing

instance (ToModelExample a, ToSwaggerModel a) => ToSwaggerModel [a] where
  toSwagModel Proxy = let p = (Proxy :: Proxy a) in
      toSwagModel p & swagModelExample .~ toExample p

class ToModelExample model where toExample :: Proxy model -> Maybe Value

data User = User {
    firstName :: String
  , age       :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON User

instance ToSwaggerModel User where
  toSwagModel Proxy =
    SwaggerModel {
        _swagModelName = Just (ModelName "User")
      , _swagProperties = [ ("firstName", StringSwag)
                          , ("age", IntegerSwag)
                          ]
      , _swagDescription = Just $ Description "User's first name"
      , _swagModelExample = Just $ toJSON $ User "dave" 30
      }

type API = "user" :> "cat" :> ReqBody '[JSON] User :> Post '[JSON] [User]

go :: IO ()
go = do
  let x = encode $ (toSwaggerDocs (Proxy :: Proxy API) defSwagRoute) {
           _swaggerInfo    = SwaggerInfo (APITitle "Servant Swagger API") (APIVersion "2.0")
                            (APIDescription "This is a an API that tests swagger integration")
                            (Just license')
        ,  _swaggerSchemes = Just [ Http ]
        }
  BL8.writeFile "foo.json" x
  where
    license' = APILicense "MIT" (Just "http://mit.com")
