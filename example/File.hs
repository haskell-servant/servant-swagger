{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module Main where

import Servant.API
import Servant.Server
import Servant.Swagger
import Data.Proxy
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import GHC.Generics
import Control.Lens

-- Types
data Todo = Todo {
    created :: Int
  , description :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

newtype TodoId = TodoId String deriving (FromText)

-- API
type API = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

-- Swagger Doc
swagDoc :: SwaggerAPI
swagDoc = swagger (Proxy :: Proxy API) mempty (BasePath "/") info schemes Nothing []
  where
    schemes = [ Http ]
    license' = APILicense "MIT" (Just "http://mit.com")
    info =
      Info
       (APITitle "Todo API") (APIVersion "1.0")
       (APIDescription "This is a an API that tests servant-swagger support for a Todo")
       (Just license')
       Nothing
       Nothing

-- Documentation and annotations
instance ToSwaggerParamType TodoId where toSwaggerParamType = const StringSwagParam
instance ToSwaggerDescription TodoId where toSwaggerDescription = const "TodoId param"

instance ToSwaggerModel Todo where
  toSwagModel Proxy =
    emptyModel
      & swagModelName .~ ModelName "Todo"
      & swagProperties .~ [ ("created", IntegerSwag)
                          , ("description", StringSwag)
                          , ("extraTodos", Model $ ModelSwag (ModelName "Todo") False)
                          ]
      & swagDescription ?~ Description "This is some real Todo right here"
      & swagModelExample ?~ toJSON (Todo 100 "get milk")
      & swagModelRequired .~ ["description"]

-- Main, create swaggger.json
main :: IO ()
main = BL8.writeFile "swagger.json" (encode swagDoc)
