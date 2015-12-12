{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant.API
import Servant.Swagger
import Servant.Server
import Data.Proxy
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Either

-- Test API
type TodoAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
  :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] (Maybe Todo)
  :<|> "todo" :> "count" :> Get '[JSON] Todo
  :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo

type TestAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

swagDoc :: SwaggerAPI
swagDoc = swagger (Proxy :: Proxy TestAPI) mempty (BasePath "/") info schemes Nothing []
  where
    schemes = [ Http ]
    license' = APILicense "MIT" (Just "http://mit.com")
    info =
      Info
       (APITitle "Todo API") (APIVersion "1.0")
       (APIDescription "This is a an API that tests servant-swagger support for a Todo API")
       (Just license')
       Nothing
       Nothing


type DocsAPI = Get '[JSON] SwaggerAPI

type API = DocsAPI :<|> TodoAPI

-- Data
data Todo = Todo { created :: Int, description :: String }
     deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

newtype TodoId = TodoId String deriving (FromText)
newtype TodoCount = TodoCount Int deriving (FromText)
newtype Completed = Completed Bool deriving (FromText)

api :: Proxy TodoAPI
api = Proxy

-- Generate Swagger Docs
main :: IO ()
main = do
  putStrLn "Running on port 8000"
  run 8000 $ serve (Proxy :: Proxy API) endpoints
  where
    endpoints = swagHandler :<|> undefined
                  undefined :<|> undefined :<|> undefined
                  undefined :<|> undefined

swagHandler :: EitherT ServantErr IO SwaggerAPI
swagHandler = pure $ swagger api mempty (BasePath "/") info schemes Nothing []
  where
    schemes = [ Http ]
    license' = APILicense "MIT" (Just "http://mit.com")
    info =
      Info
       (APITitle "Servant Swagger API") (APIVersion "2.0")
       (APIDescription "This is a an API that tests swagger integration")
       (Just license')
       Nothing
       Nothing

-- Instances
instance ToSwaggerModel Todo where
  toSwagModel Proxy =
    SwaggerModel {
        _swagModelName = (ModelName "Todo")
      , _swagProperties = [ ("created", IntegerSwag)
                          , ("description", StringSwag)
                          ]
      , _swagDescription = Just $ Description "This is some real Todo right here"
      , _swagModelExample = Just $ toJSON $ Todo 100 "get milk"
      , _swagModelRequired = ["description"]
      }

instance ToSwaggerParamType TodoId where
  toSwaggerParamType = const StringSwagParam

instance ToSwaggerParamType Completed where
  toSwaggerParamType = const BooleanSwagParam

instance ToSwaggerDescription TodoId where
  toSwaggerDescription = const "TodoId param"

instance ToSwaggerDescription Completed where
  toSwaggerDescription = const "Completed param"
