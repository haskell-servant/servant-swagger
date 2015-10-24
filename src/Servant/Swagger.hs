module Servant.Swagger 
  (
  -- * Classes
    HasSwagger           (..)
  , ToSwaggerDescription (..)
  , ToResponseHeader     (..)
  , ToSwaggerParamType   (..)
  , SwaggerParamType     (..)
  , ToSwaggerModel       (..)
  , ToModelExample       (..)
  -- * Types
  , APIDescription       (..)
  , APITermsOfService    (..)
  , SwaggerAPI           (..)
  , SwaggerPath          (..)
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
  -- * Swaggadelic
  , swagger
  , emptySwaggerModel
  ) where

import Servant.Swagger.Internal
import Data.Proxy

swagger :: HasSwagger swagger => Proxy swagger -> BasePath -> Info -> [Scheme] -> SwaggerAPI
swagger proxy basePath info schemes =
  let SwagResult{..} = toSwaggerDocs proxy defSwagRoute
  in SwaggerAPI info schemes _resultPaths _resultModels [] (setPath basePath)




