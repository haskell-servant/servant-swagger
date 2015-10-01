{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Servant.Swagger where

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
instance SwaggerAccept FormEncoded where toSwaggerAccept Proxy = FormEncoded
instance SwaggerAccept PlainText where toSwaggerAccept Proxy = PlainText

class SwaggerAcceptTypes (xs :: [*]) where toSwaggerAcceptTypes :: Proxy xs -> [ContentType]
instance SwaggerAcceptTypes '[] where toSwaggerAcceptTypes Proxy = []

instance (SwaggerAccept x, SwaggerAcceptTypes xs) => SwaggerAcceptTypes (x ': xs) where
  toSwaggerAcceptTypes Proxy =
    toSwaggerAccept (Proxy :: Proxy x) :
      toSwaggerAcceptTypes (Proxy :: Proxy xs)

class ToVerb a where toVerb :: Proxy a -> Verb
instance ToVerb Get where toVerb Proxy   = Get
instance ToVerb Put where toVerb Proxy   = Put
instance ToVerb Patch where toVerb Proxy = Patch
instance ToVerb Head where toVerb Proxy  = Head
instance ToVerb Post where toVerb Proxy  = Post
instance ToVerb Delete where toVerb Proxy  = Delete
instance ToVerb Options where toVerb Proxy  = Options

instance (ToVerb verb, SwaggerAcceptTypes xs) => HasSwagger (verb xs returnType) where
  toSwaggerDocs Proxy swagRoute =
    let swagPath = SwaggerPath [(toVerb (Proxy :: Proxy verb), path)]
        path = Path mempty (swagRoute ^. routeParams) [(200, Response "Success")]
                  (toSwaggerAcceptTypes (Proxy :: Proxy xs)) (swagRoute ^. routeConsumes)
    in SwaggerAPI (swagRoute ^. routeSwagInfo) (swagRoute ^. routeSwagSchemes)
      [(swagRoute ^. routePathName, swagPath)] (swagRoute ^. routeModels)

class ToSwaggerType a where
  toSwaggerType :: Proxy a -> SwaggerType

instance ToSwaggerType Bool where toSwaggerType Proxy = BoolSwag
instance ToSwaggerType a => ToSwaggerType [a] where toSwaggerType Proxy = ArraySwag

class ToSwaggerDescription a where toSwaggerDescription :: Proxy a -> Text

instance (ToSwaggerDescription typ, ToSwaggerType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (Capture sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newPath = PathName $ mconcat ["/{",pName,"}"]
        newParam = Param PathUrl pName
                     (Just $ toSwaggerType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True Nothing
        newSwagRoute = swagRoute & routePathName %~ (<>) newPath
                                 & routeParams %~ (:) newParam

instance (ToSwaggerDescription typ, ToSwaggerType typ, KnownSymbol sym, HasSwagger rest) =>
  HasSwagger (QueryParam sym typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
      where
        pName = T.pack $ symbolVal (Proxy :: Proxy sym)
        newParam = Param Query pName
                     (Just $ toSwaggerType (Proxy :: Proxy typ)) Nothing
                       (toSwaggerDescription (Proxy :: Proxy typ)) True Nothing
        newSwagRoute = swagRoute & routeParams %~ (:) newParam

------------------------------------------------------------------------------
-- | Swagger doesn't support query flags, bypass
instance (ToSwaggerDescription typ, ToSwaggerType typ, HasSwagger rest) =>
  HasSwagger (QueryFlag typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) swagRoute

------------------------------------------------------------------------------
-- | Swagger doesn't support matrix flags, bypass
instance (ToSwaggerDescription typ, ToSwaggerType typ, HasSwagger rest) =>
  HasSwagger (MatrixParam typ :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) swagRoute

------------------------------------------------------------------------------
-- | ReqBody
instance (SwaggerAcceptTypes ctypes, ToSwaggerModel model, HasSwagger rest) =>
  HasSwagger (ReqBody ctypes model :> rest) where
    toSwaggerDocs Proxy swagRoute = toSwaggerDocs (Proxy :: Proxy rest) newSwagRoute
       where
         SwaggerModel {..} = toSwagModel (Proxy :: Proxy model)
         name = unModelName _swagModelName
         newSwagRoute =
           swagRoute & routeModels %~ H.insert _swagModelName (toSwagModel (Proxy :: Proxy model))
                     & routeParams %~ (:) newParam
                     & routeConsumes %~ (++) (toSwaggerAcceptTypes (Proxy :: Proxy ctypes))
         newParam = Param Body name Nothing Nothing
             (fromMaybe mempty (unDescription <$> _swagDescription)) True Nothing

-- testing
-- type API = "user" :> Capture "userid" :> Post '[JSON] ()

-- main :: IO ()
-- main = print $ toSwaggerDocs (Proxy :: Proxy API) defSwagRoute
