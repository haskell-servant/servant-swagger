{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import GHC.Generics
import Servant
import Servant.Swagger

-- Types
data Todo = Todo
  { created     :: Int
  , description :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Todo

newtype TodoId = TodoId String deriving (FromText, Generic)

-- API
type API = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

-- Swagger Doc
swagDoc :: Swagger
swagDoc = toSwagger (Proxy :: Proxy API)
  & info.infoTitle   .~ "Todo API"
  & info.infoVersion .~ "1.0"
  & info.infoDescription ?~ "This is an API that tests servant-swagger support for a Todo"
  & info.infoLicense ?~ License "MIT" (Just (URL "http://mit.com"))

-- Documentation and annotations
instance ToParamSchema TodoId

instance ToSchema Todo where
  declareNamedSchema proxy = do
    (name, schema) <- genericDeclareNamedSchema defaultSchemaOptions proxy
    return (name, schema
      & schemaDescription ?~ "This is some real Todo right here"
      & schemaExample ?~ toJSON (Todo 100 "get milk"))

-- Main, create swaggger.json
main :: IO ()
main = BL8.writeFile "swagger.json" (encode swagDoc)

