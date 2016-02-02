{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger

-- Test API
type TodoAPI
    = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
 :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] (Maybe Todo)
 :<|> "todo" :> "count" :> Get '[JSON] Todo
 :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo

type TestAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

swagDoc :: Swagger
swagDoc = toSwagger (Proxy :: Proxy TestAPI)
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests servant-swagger support for a Todo"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

type DocsAPI = Get '[JSON] Swagger

type API = DocsAPI :<|> TodoAPI

-- Data
data Todo = Todo { created :: Int, summary :: String }
     deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

newtype TodoId = TodoId String deriving (FromText, Generic)
newtype TodoCount = TodoCount Int deriving (FromText, Generic)
newtype Completed = Completed Bool deriving (FromText, Generic)

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

swagHandler :: EitherT ServantErr IO Swagger
swagHandler = pure $ toSwagger api
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

instance ToSchema Todo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Todo right here"
    & mapped.schema.example ?~ toJSON (Todo 100 "get milk")

instance ToParamSchema TodoId

instance ToParamSchema Completed

