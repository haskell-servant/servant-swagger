{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import Control.Lens
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Web.HttpApiData

-- Test API
type TodoAPI
    = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
 :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] (Maybe Todo)
 :<|> "todo" :> "count" :> Get '[JSON] Todo
 :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo

type TestAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

swagDoc :: Swagger
swagDoc = toSwagger (Proxy :: Proxy TestAPI)
  & info.infoTitle   .~ "Todo API"
  & info.infoVersion .~ "1.0"
  & info.infoDescription ?~ "This is an API that tests servant-swagger support for a Todo"
  & info.infoLicense ?~ License "MIT" (Just (URL "http://mit.com"))

type DocsAPI = Get '[JSON] Swagger

type API = DocsAPI :<|> TodoAPI

-- Data
data Todo = Todo { created :: Int, description :: String }
     deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

newtype TodoId = TodoId String deriving (Generic,FromHttpApiData)
newtype TodoCount = TodoCount Int deriving (Generic,FromHttpApiData)
newtype Completed = Completed Bool deriving (Generic,FromHttpApiData)

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

swagHandler :: ExceptT ServantErr IO Swagger
swagHandler = pure $ toSwagger api
  & info.infoTitle   .~ "Todo API"
  & info.infoVersion .~ "1.0"
  & info.infoDescription ?~ "This is an API that tests swagger integration"
  & info.infoLicense ?~ License "MIT" (Just (URL "http://mit.com"))

-- Instances
instance ToSchema Todo where
  declareNamedSchema proxy = do
    (name, schema) <- genericDeclareNamedSchema defaultSchemaOptions proxy
    return (name, schema
      & schemaDescription ?~ "This is some real Todo right here"
      & schemaExample ?~ toJSON (Todo 100 "get milk"))

instance ToParamSchema TodoId

instance ToParamSchema Completed

