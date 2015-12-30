<h1 align="center">
    <a href="https://github.com/dmjio/servant-swagger">
        servant-swagger
    </a>
    <br/>
    <a href="http://hackage.haskell.org/package/servant-swagger">
      <img alt="Hackage" src="https://img.shields.io/hackage/v/servant-swagger.svg" />
    </a>
</h1>

<p align="center">
  This project converts <a href="https://github.com/haskell-servant/servant">servant</a> APIs into Swagger 2.0 conforming JSON.
</p>

<p align="center">
  <img src="http://s16.postimg.org/rndz1wbyt/servant.png" />
</p>

<hr>

Given the following `servant` API, `servant-swagger` generates the following json.

### [Input](example/File.hs)
```haskell
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
```

### Output

```json
{
   "swagger":"2.0",
   "info":{
      "version":"1.0",
      "title":"Todo API",
      "license":{
         "url":"http://mit.com",
         "name":"MIT"
      },
      "description":"This is an API that tests servant-swagger support for a Todo"
   },
   "definitions":{
      "Todo":{
         "example":{
            "created":100,
            "description":"get milk"
         },
         "required":[
            "created",
            "description"
         ],
         "type":"object",
         "description":"This is some real Todo right here",
         "properties":{
            "created":{
               "maximum":9223372036854775807,
               "minimum":-9223372036854775808,
               "type":"integer"
            },
            "description":{
               "type":"string"
            }
         }
      }
   },
   "paths":{
      "/todo/{id}":{
         "get":{
            "responses":{
               "404":{
                  "description":"id not found"
               },
               "200":{
                  "schema":{
                     "$ref":"#/definitions/Todo"
                  },
                  "description":""
               }
            },
            "produces":[
               "application/json"
            ],
            "parameters":[
               {
                  "required":true,
                  "in":"path",
                  "name":"id",
                  "type":"string"
               }
            ]
         }
      }
   }
}
```
## Try it out
 - All generated swagger docs can be interactively viewed on <a href="http://editor.swagger.io/">Swagger Editor</a>

## Limitations
 - Quite a few, TODO: add this

## FAQ
- Q: How is this project different from the `swagger` package on `hackage` ?
  - A: This package is based on the latest Swagger 2.0 API
