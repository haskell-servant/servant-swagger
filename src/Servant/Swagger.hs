{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Servant.Swagger where

import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson
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
import           GHC.Exts (Constraint)
import           GHC.TypeLits
import           Servant.API 
import           Servant.Swagger.Internal

class HasSwagger h where toSwaggerDocs :: Proxy h -> SwagRoute -> SwaggerAPI

instance (HasSwagger rest, KnownSymbol sym) => HasSwagger (sym :> rest) where
  toSwaggerDocs Proxy swagRoute =
    toSwaggerDocs (Proxy :: Proxy rest) (swagRoute & routePathName %~ (<> path))
    where path = PathName $ "/" <> T.pack (symbolVal (Proxy :: Proxy sym))

instance (HasSwagger left, HasSwagger right) => HasSwagger (left :<|> right) where
  toSwaggerDocs Proxy swagRoute@(SwagRoute _ _  _ _  info schemes) =
    let swagLeft = toSwaggerDocs (Proxy :: Proxy left) swagRoute
        swagRight = toSwaggerDocs (Proxy :: Proxy right) swagRoute
        paths = H.unionWith f (swagLeft ^. swaggerPaths) (swagRight ^. swaggerPaths)
        models = H.union (swagLeft ^. swaggerDefinitions) (swagRight ^. swaggerDefinitions)
    in SwaggerAPI info schemes paths models
      where f (SwaggerPath l) (SwaggerPath r) = SwaggerPath (H.union l r)

class SwaggerAccept a where toSwaggerAccept :: Proxy a -> ContentType
instance SwaggerAccept JSON where toSwaggerAccept Proxy = JSON
instance SwaggerAccept HTML where toSwaggerAccept Proxy = HTML
instance SwaggerAccept XML where toSwaggerAccept Proxy = XML
instance SwaggerAccept FormUrlEncoded where toSwaggerAccept Proxy = FormUrlEncoded
instance SwaggerAccept PlainText where toSwaggerAccept Proxy = PlainText
instance SwaggerAccept OctetStream where toSwaggerAccept Proxy = OctetStream

class SwaggerAcceptTypes (xs :: [*]) where toSwaggerAcceptTypes :: Proxy xs -> [ContentType]
instance SwaggerAcceptTypes '[] where toSwaggerAcceptTypes Proxy = []

instance (SwaggerAccept x, SwaggerAcceptTypes xs) => SwaggerAcceptTypes (x ': xs) where
  toSwaggerAcceptTypes Proxy = toSwaggerAccept (Proxy :: Proxy x) : toSwaggerAcceptTypes (Proxy :: Proxy xs)

class ToVerb a where toVerb :: Proxy a -> Verb
instance ToVerb Get where toVerb Proxy   = Get
instance ToVerb Put where toVerb Proxy   = Put
instance ToVerb Patch where toVerb Proxy = Patch
instance ToVerb Head where toVerb Proxy  = Head
instance ToVerb Post where toVerb Proxy  = Post
instance ToVerb Delete where toVerb Proxy  = Delete
instance ToVerb Options where toVerb Proxy  = Options

instance (ToSwaggerModel returnType, ToVerb verb, SwaggerAcceptTypes xs) => HasSwagger (verb xs returnType) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path mempty (swagRoute ^. routeParams) [(200, Response "Success" (swagModel ^. swagModelName))]
                  (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes)
    in SwaggerAPI (swagRoute ^. routeSwagInfo) (swagRoute ^. routeSwagSchemes)
      [(swagRoute ^. routePathName, swagPath)] newModels
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
                       (toSwaggerDescription (Proxy :: Proxy typ)) True True Nothing
        newSwagRoute = swagRoute & routePathName %~ flip (<>) newPath
                                 & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParam sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just $ toSwaggerParamType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerParamType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParams sym [typ] :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just ArraySwagParam) (Just $ ItemObject (toSwaggerParamType (Proxy :: Proxy typ)))
                       (toSwaggerDescription (Proxy :: Proxy typ)) True False Nothing
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Query Flag
instance (ToSwaggerDescription sym, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryFlag sym :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     Nothing Nothing
                       (toSwaggerDescription (Proxy :: Proxy sym)) True True Nothing
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Swagger doesn't support matrix params, bypass
instance (ToSwaggerDescription typ, ToSwaggerParamType typ, HasSwagger rest) =>
  HasSwagger (MatrixParam typ :> rest) where
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
        newParams = Param Servant.Swagger.Internal.Header pName (Just typ) Nothing pDesc False True Nothing

------------------------------------------------------------------------------
-- | Swagger Response Headers
-- instance (ToSwaggerDescription typ, ToSwaggerParamType typ, HasSwagger rest) =>
--   HasSwagger (Headers xs typ :> rest) where
--     toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
--       where
--         newSwagRoute = swagRoute & routeParams %~ (:) newParams
--         pName = T.pack $ symbolVal (Proxy :: Proxy sym) 
--         pDesc = toSwaggerDescription (Proxy :: Proxy typ) 
--         typ = toSwaggerParamType (Proxy :: Proxy typ) 
--         newParams = Param Servant.Swagger.Internal.Header pName (Just typ) Nothing pDesc False True Nothing

------------------------------------------------------------------------------
-- | ReqBody
instance (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
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
                (fromMaybe mempty (unDescription <$> _swagDescription)) True False Nothing ]

-- Testing
instance ToSwaggerParamType Int where toSwaggerParamType = const IntegerSwagParam
instance ToSwaggerParamType String where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Text where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType BL8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType B8.ByteString where toSwaggerParamType = const StringSwagParam
instance ToSwaggerParamType Double where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Float where toSwaggerParamType = const NumberSwagParam
instance ToSwaggerParamType Bool where toSwaggerParamType Proxy = BooleanSwagParam

instance ToSwaggerDescription String where toSwaggerDescription _ = "foo header"
instance ToSwaggerDescription Int where toSwaggerDescription = const "UserId of User"
instance ToSwaggerDescription "foo" where toSwaggerDescription = const "foo flag"

instance ToSwaggerModel () where toSwagModel Proxy = SwaggerModel Nothing [] Nothing

instance ToSwaggerModel User where
  toSwagModel Proxy =
    SwaggerModel {
        _swagModelName = Just (ModelName "User")
      , _swagProperties = [ ("firstName", StringSwag)
                          , ("age", IntegerSwag)
                          ]
      , _swagDescription = Just $ Description "User's first name"
      }

newtype DOB = DOB Int deriving (Num, Show)

data User = User {
    firstName :: String
  , age :: Integer
  , dob :: DOB
  } deriving (Typeable, Show)

type API = "user" :> "cat" :> Post '[JSON] User
      :<|> "user" :> Capture "userid" Int :> "happy" :> QueryParam "huh" Int :> Get '[JSON] ()
      :<|> "user" :> Capture "userid" Int :> ReqBody '[JSON] User :> QueryFlag "foo" :> Delete '[JSON] ()
      :<|> "user" :> Capture "userid" Int :> Header "foo" String :> Put '[JSON] ()

go :: IO ()
go = do
  let x = encode $ toSwaggerDocs (Proxy :: Proxy API) defSwagRoute
  BL8.writeFile "foo.json" x
