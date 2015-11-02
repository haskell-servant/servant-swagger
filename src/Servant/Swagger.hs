module Servant.Swagger 
  (
  -- * Classes
    HasSwagger           (..)
  , ToSwaggerDescription (..)
  , ToHeader             (..)
  , ToSwaggerParamType   (..)
  , SwaggerParamType     (..)
  , ToSwaggerModel       (..)
  , ToHeaderDescription  (..)
  , ToModelExample       (..)
  -- * Types
  , APIDescription       (..)
  , Contact              (..)
  , ContactName          (..)
  , ContactURL           (..)
  , ContactEmail         (..)
  , TermsOfService       (..)
  , SwaggerAPI           (..)
  , SwaggerPath          (..)
  , SwaggerRouteInfo     (..)
  , Path                 (..)
  , Code                 (..)
  , Verb                 (..)
  , PathSummary          (..)
  , SwaggerType          (..)
  , SwaggerModel         (..)
  , Info                 (..)
  , ModelName            (..)
  , ContentType          (..)
  , APIVersion           (..)
  , APITitle             (..)
  , APILicense           (..)
  , Scheme               (..)
  , Description          (..)
  , BasePath             (..)
  , Response             (..)
  , ModelSwag            (..)
  , SwaggerHeader        (..)
  , responseDescription
  , responseModelName
  , responseHeaders
  , responseIsArray
  , responseCode
  , defResponse
  , Tag                  (..)
  , TagName              (..)
  , TagDescription       (..)
  , tagName
  , tagDescription
  -- * Swaggadelic
  , swagger
  , emptyModel
  , swaggerPathInfo
  , emptyRouteDescription
  -- * Lenses
  , swagModelName
  , swagModelExample
  , swagProperties
  , swagModelRequired
  , swagDescription
  , swagRouteTags
  , swagRouteSummary
  , swagRouteResponses
  , swagRouteModels
  , PathDescription (..)
  , swagRouteDescription
  , OperationId (..)
  , swagRouteOperationId
  , defSwaggerInfo
  ) where

import Servant.Swagger.Internal
import Data.Proxy
import Data.Monoid

swagger
  :: HasSwagger swagger
  => Proxy swagger
  -> SwaggerRouteInfo swagger
  -> BasePath
  -> Info
  -> [Scheme]
  -> Maybe HostName
  -> [SecurityDefinition]
  -> SwaggerAPI
swagger proxy (SwaggerRouteInfo routeInfo) basePath info schemes hostName secDefs = do
  let result@SwagResult{..} = routeInfo <> toSwaggerDocs proxy defSwaggerRoute
  SwaggerAPI info schemes _resultPaths _resultModels
    (getAllTags result) (setPath basePath) hostName secDefs




