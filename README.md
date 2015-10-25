<h1 align="center">
    <a href="https://github.com/dmjio/servant-swagger">
        servant-swagger
    </a>
</h1>

<p align="center">
  This project converts <a href="">servant</a> APIs into Swagger 2.0 conforming JSON.
</p>

<p align="center">
  <img src="http://s16.postimg.org/rndz1wbyt/servant.png" />
</p>  

<hr>

Given the following `servant` API, `servant-swagger` generates the following json.

### Input 

```haskell
import Servant.API
import Servant.Swagger
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8

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
swagDoc = swagger (Proxy :: Proxy API) (BasePath "/") info
  where
    schemes = [ Http ]
    license' = APILicense "MIT" (Just "http://mit.com")
    info =
      Info
       (APITitle "Todo API") (APIVersion "1.0")
       (APIDescription "This is a an API that tests servant-swagger support for a Todo")
       (Just license')

-- Documentation and annotations
instance ToSwaggerParamType TodoId where toSwaggerParamType = const StringSwagParam  
instance ToSwaggerDescription TodoId where toSwaggerDescription = const "TodoId param" 

-- Main, create swaggger.json
main :: IO ()
main = BL8.writeFile "swagger.json" (encode swagDog)
```

### Output

```json
{
    "swagger": "2.0",
    "basePath": "/",
    "schemes": [
        "http"
    ],
    "info": {
        "version": "1.0",
        "title": "Todo API",
        "license": {
            "url": "http://mit.com",
            "name": "MIT"
        },
        "description": "This is a an API that tests servant-swagger support for a Todo API"
    },
    "definitions": {
        "Todo": {
            "example": {
                "created": 100,
                "description": "get milk"
            },
            "type": "object",
            "description": "This is some real Todo right here",
            "properties": {
                "created": {
                    "format": "int32",
                    "type": "integer"
                },
                "description": {
                    "type": "string"
                }
            }
        }
    },
    "paths": {
        "/todo/{id}": {
            "get": {
                "summary": "",
                "consumes": [],
                "responses": {
                    "200": {
                        "schema": {
                            "$ref": "#/definitions/Todo"
                        },
                        "headers": {},
                        "description": "OK"
                    }
                },
                "produces": [
                    "application/json"
                ],
                "parameters": [
                    {
                        "required": true,
                        "in": "path",
                        "name": "id",
                        "type": "string",
                        "description": "TodoId param"
                    }
                ],
                "tags": [
                    "todo"
                ]
            }
        }
    },
    "tags": []
}
```

## Limitations
 - Quite a few, TODO: add this

## FAQ
- Q: How is this project different from the `swagger` package on `hackage` ?
  - A: This package is based on the latest Swagger 2.0 API