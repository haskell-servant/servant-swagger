{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Servant.SwaggerSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Char (toLower)
import Data.Proxy
import Data.Swagger hiding (Tag(..))
import Data.Time
import GHC.Generics
import Servant.API
import Servant.Swagger
import Test.Hspec

checkAPI :: HasSwagger api => Proxy api -> Value -> IO ()
checkAPI proxy spec = toJSON (toSwagger proxy) `shouldBe` spec

spec :: Spec
spec = describe "HasSwagger" $
  it "Todo API" $ checkAPI (Proxy :: Proxy TodoAPI) todoAPI

main :: IO ()
main = hspec spec

-- =======================================================================
-- Todo API
-- =======================================================================

data Todo = Todo
  { created     :: UTCTime
  , title       :: String
  , description :: Maybe String
  } deriving (Generic, FromJSON, ToSchema)

newtype TodoId = TodoId String deriving (Generic, FromText, ToParamSchema)

type TodoAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

todoAPI :: Value
todoAPI = [aesonQQ|
{
  "swagger":"2.0",
  "info":
    {
      "title": "",
      "version": ""
    },
  "definitions":
    {
      "Todo":
        {
          "type": "object",
          "required": [ "created", "title" ],
          "properties":
            {
              "created": { "$ref": "#/definitions/UTCTime" },
              "title": { "type": "string" },
              "description": { "type": "string" }
            }
        },
      "UTCTime":
        {
          "type": "string",
          "format": "yyyy-mm-ddThh:MM:ssZ"
        }
    },
  "paths":
    {
      "/todo/{id}":
        {
          "get":
            {
              "responses":
                {
                  "200":
                    {
                      "schema": { "$ref":"#/definitions/Todo" },
                      "description": ""
                    },
                  "404": { "description": "id not found" }
                },
              "produces": [ "application/json" ],
              "parameters":
                [
                  {
                    "required": true,
                    "in": "path",
                    "name": "id",
                    "type": "string"
                   }
                ]
            }
        }
    }
}
|]

