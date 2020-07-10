{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PackageImports     #-}
module Servant.SwaggerSpec where

import           Control.Lens
import           Data.Aeson       (ToJSON(toJSON), Value, genericToJSON, encode)
import           Data.Aeson.QQ.Simple
import qualified Data.Aeson.Types as JSON
import           Data.Char        (toLower)
import           Data.Int         (Int64)
import           Data.Proxy
import           Data.Swagger
import           Data.Text        (Text)
import           Data.Time
import           GHC.Generics
import           Servant.API
import           Servant.Swagger
import           Servant.Test.ComprehensiveAPI (comprehensiveAPI)
import           Test.Hspec       hiding (example)

checkAPI :: HasCallStack => HasSwagger api => Proxy api -> Value -> IO ()
checkAPI proxy = checkSwagger (toSwagger proxy)

checkSwagger :: HasCallStack => Swagger -> Value -> IO ()
checkSwagger swag js = encode (toJSON swag) `shouldBe` (encode js)

spec :: Spec
spec = describe "HasSwagger" $ do
  it "Todo API" $ checkAPI (Proxy :: Proxy TodoAPI) todoAPI
  it "Hackage API (with tags)" $ checkSwagger hackageSwaggerWithTags hackageAPI
  it "GetPost API (test subOperations)" $ checkSwagger getPostSwagger getPostAPI
  it "Comprehensive API" $ do
    let _x = toSwagger comprehensiveAPI
    True `shouldBe` True -- type-level test

main :: IO ()
main = hspec spec

-- =======================================================================
-- Todo API
-- =======================================================================

data Todo = Todo
  { created :: UTCTime
  , title   :: String
  , summary :: Maybe String
  } deriving (Generic)

instance ToJSON Todo
instance ToSchema Todo

newtype TodoId = TodoId String deriving (Generic)
instance ToParamSchema TodoId

type TodoAPI = "todo" :> Capture "id" TodoId :> Get '[JSON] Todo

todoAPI :: Value
todoAPI = [aesonQQ|
{
  "openapi": "3.0.0",
  "info": {
    "version": "",
    "title": ""
  },
  "components": {
    "schemas": {
      "Todo": {
        "required": [
          "created",
          "title"
        ],
        "type": "object",
        "properties": {
          "summary": {
            "type": "string"
          },
          "created": {
            "$ref": "#/components/schemas/UTCTime"
          },
          "title": {
            "type": "string"
          }
        }
      },
      "UTCTime": {
        "example": "2016-07-22T00:00:00Z",
        "format": "yyyy-mm-ddThh:MM:ssZ",
        "type": "string"
      }
    }
  },
  "paths": {
    "/todo/{id}": {
      "get": {
        "responses": {
          "404": {
            "description": "`id` not found"
          },
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "$ref": "#/components/schemas/Todo"
                }
              }
            },
            "description": ""
          }
        },
        "parameters": [
          {
            "required": true,
            "schema": {
              "type": "string"
            },
            "in": "path",
            "name": "id"
          }
        ]
      }
    }
  }
}
|]

-- =======================================================================
-- Hackage API
-- =======================================================================

type HackageAPI
    = HackageUserAPI
 :<|> HackagePackagesAPI

type HackageUserAPI =
      "users" :> Get '[JSON] [UserSummary]
 :<|> "user"  :> Capture "username" Username :> Get '[JSON] UserDetailed

type HackagePackagesAPI
    = "packages" :> Get '[JSON] [Package]

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int64  -- Word64 would make sense too
  } deriving (Eq, Show, Generic)

lowerCutPrefix :: String -> String -> String
lowerCutPrefix s = map toLower . drop (length s)

instance ToJSON UserSummary where
  toJSON = genericToJSON JSON.defaultOptions { JSON.fieldLabelModifier = lowerCutPrefix "summary" }

instance ToSchema UserSummary where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = lowerCutPrefix "summary" } proxy
    & mapped.schema.example ?~ toJSON UserSummary
         { summaryUsername = "JohnDoe"
         , summaryUserid   = 123 }

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int64
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)
instance ToSchema UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)
instance ToSchema Package

hackageSwaggerWithTags :: Swagger
hackageSwaggerWithTags = toSwagger (Proxy :: Proxy HackageAPI)
  & servers .~ ["https://hackage.haskell.org"]
  & applyTagsFor usersOps    ["users"    & description ?~ "Operations about user"]
  & applyTagsFor packagesOps ["packages" & description ?~ "Query packages"]
  where
    usersOps, packagesOps :: Traversal' Swagger Operation
    usersOps    = subOperations (Proxy :: Proxy HackageUserAPI)     (Proxy :: Proxy HackageAPI)
    packagesOps = subOperations (Proxy :: Proxy HackagePackagesAPI) (Proxy :: Proxy HackageAPI)

hackageAPI :: Value
hackageAPI = [aesonQQ|
{
  "openapi": "3.0.0",
  "servers": [
    {
      "url": "https://hackage.haskell.org"
    }
  ],
  "components": {
    "schemas": {
      "UserDetailed": {
        "required": [
          "username",
          "userid",
          "groups"
        ],
        "type": "object",
        "properties": {
          "groups": {
            "items": {
              "type": "string"
            },
            "type": "array"
          },
          "username": {
            "type": "string"
          },
          "userid": {
            "maximum": 9223372036854775807,
            "format": "int64",
            "minimum": -9223372036854775808,
            "type": "integer"
          }
        }
      },
      "Package": {
        "required": [
          "packageName"
        ],
        "type": "object",
        "properties": {
          "packageName": {
            "type": "string"
          }
        }
      },
      "UserSummary": {
        "example": {
          "username": "JohnDoe",
          "userid": 123
        },
        "required": [
          "username",
          "userid"
        ],
        "type": "object",
        "properties": {
          "username": {
            "type": "string"
          },
          "userid": {
            "maximum": 9223372036854775807,
            "format": "int64",
            "minimum": -9223372036854775808,
            "type": "integer"
          }
        }
      }
    }
  },
  "info": {
    "version": "",
    "title": ""
  },
  "paths": {
    "/users": {
      "get": {
        "responses": {
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "items": {
                    "$ref": "#/components/schemas/UserSummary"
                  },
                  "type": "array"
                }
              }
            },
            "description": ""
          }
        },
        "tags": [
          "users"
        ]
      }
    },
    "/packages": {
      "get": {
        "responses": {
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "items": {
                    "$ref": "#/components/schemas/Package"
                  },
                  "type": "array"
                }
              }
            },
            "description": ""
          }
        },
        "tags": [
          "packages"
        ]
      }
    },
    "/user/{username}": {
      "get": {
        "responses": {
          "404": {
            "description": "`username` not found"
          },
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "$ref": "#/components/schemas/UserDetailed"
                }
              }
            },
            "description": ""
          }
        },
        "parameters": [
          {
            "required": true,
            "schema": {
              "type": "string"
            },
            "in": "path",
            "name": "username"
          }
        ],
        "tags": [
          "users"
        ]
      }
    }
  },
  "tags": [
    {
      "name": "users",
      "description": "Operations about user"
    },
    {
      "name": "packages",
      "description": "Query packages"
    }
  ]
}
|]


-- =======================================================================
-- Get/Post API (test for subOperations)
-- =======================================================================

type GetPostAPI = Get '[JSON] String :<|> Post '[JSON] String

getPostSwagger :: Swagger
getPostSwagger = toSwagger (Proxy :: Proxy GetPostAPI)
  & applyTagsFor getOps ["get" & description ?~ "GET operations"]
  where
    getOps :: Traversal' Swagger Operation
    getOps = subOperations (Proxy :: Proxy (Get '[JSON] String)) (Proxy :: Proxy GetPostAPI)

getPostAPI :: Value
getPostAPI = [aesonQQ|
{
  "components": {},
  "openapi": "3.0.0",
  "info": {
    "version": "",
    "title": ""
  },
  "paths": {
    "/": {
      "post": {
        "responses": {
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "type": "string"
                }
              }
            },
            "description": ""
          }
        }
      },
      "get": {
        "responses": {
          "200": {
            "content": {
              "application/json;charset=utf-8": {
                "schema": {
                  "type": "string"
                }
              }
            },
            "description": ""
          }
        },
        "tags": [
          "get"
        ]
      }
    }
  },
  "tags": [
    {
      "name": "get",
      "description": "GET operations"
    }
  ]
}
|]

