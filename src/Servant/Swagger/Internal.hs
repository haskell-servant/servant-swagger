{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances #-}
#endif
module Servant.Swagger.Internal where

import Prelude ()
import Prelude.Compat

import           Control.Lens
import           Data.Aeson
import           Data.HashMap.Strict.InsOrd             (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd             as InsOrdHashMap
import           Data.Foldable (toList)
import           Data.Proxy
import           Data.Singletons.Bool
import           Data.Swagger                           hiding (Header, contentType)
import qualified Data.Swagger                           as Swagger
import           Data.Swagger.Declare
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import           GHC.TypeLits
import           Network.HTTP.Media                     (MediaType)
import           Servant.API
import           Servant.API.Description                (FoldDescription,
                                                         reflectDescription)
import           Servant.API.Modifiers                  (FoldRequired)

import           Servant.Swagger.Internal.TypeLevel.API

-- | Generate a Swagger specification for a servant API.
--
-- To generate Swagger specification, your data types need
-- @'ToParamSchema'@ and/or @'ToSchema'@ instances.
--
-- @'ToParamSchema'@ is used for @'Capture'@, @'QueryParam'@ and @'Header'@.
-- @'ToSchema'@ is used for @'ReqBody'@ and response data types.
--
-- You can easily derive those instances via @Generic@.
-- For more information, refer to <http://hackage.haskell.org/package/swagger2/docs/Data-Swagger.html swagger2 documentation>.
--
-- Example:
--
-- @
-- newtype Username = Username String deriving (Generic, ToText)
--
-- instance ToParamSchema Username
--
-- data User = User
--   { username :: Username
--   , fullname :: String
--   } deriving (Generic)
--
-- instance ToJSON User
-- instance ToSchema User
--
-- type MyAPI = QueryParam "username" Username :> Get '[JSON] User
--
-- mySwagger :: Swagger
-- mySwagger = toSwagger (Proxy :: Proxy MyAPI)
-- @
class HasSwagger api where
  -- | Generate a Swagger specification for a servant API.
  toSwagger :: Proxy api -> Swagger

instance HasSwagger Raw where
  toSwagger _ = mempty & paths . at "/" ?~ mempty

instance HasSwagger EmptyAPI where
  toSwagger _ = mempty

-- | All operations of sub API.
-- This is similar to @'operationsOf'@ but ensures that operations
-- indeed belong to the API at compile time.
subOperations :: (IsSubAPI sub api, HasSwagger sub) =>
  Proxy sub     -- ^ Part of a servant API.
  -> Proxy api  -- ^ The whole servant API.
  -> Traversal' Swagger Operation
subOperations sub _ = operationsOf (toSwagger sub)

-- | Make a singleton Swagger spec (with only one endpoint).
-- For endpoints with no content see 'mkEndpointNoContent'.
mkEndpoint :: forall a cs hs proxy method status.
  (ToSchema a, AllAccept cs, AllToResponseHeader hs, SwaggerMethod method, KnownNat status)
  => FilePath                                       -- ^ Endpoint path.
  -> proxy (Verb method status cs (Headers hs a))  -- ^ Method, content-types, headers and response.
  -> Swagger
mkEndpoint path proxy
  = mkEndpointWithSchemaRef (Just ref) path proxy
      & components.schemas .~ defs
  where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty

-- | Make a singletone 'Swagger' spec (with only one endpoint) and with no content schema.
mkEndpointNoContent :: forall nocontent cs hs proxy method status.
  (AllAccept cs, AllToResponseHeader hs, SwaggerMethod method, KnownNat status)
  => FilePath                                               -- ^ Endpoint path.
  -> proxy (Verb method status cs (Headers hs nocontent))  -- ^ Method, content-types, headers and response.
  -> Swagger
mkEndpointNoContent path proxy
  = mkEndpointWithSchemaRef Nothing path proxy

-- | Like @'mkEndpoint'@ but with explicit schema reference.
-- Unlike @'mkEndpoint'@ this function does not update @'definitions'@.
mkEndpointWithSchemaRef :: forall cs hs proxy method status a.
  (AllAccept cs, AllToResponseHeader hs, SwaggerMethod method, KnownNat status)
  => Maybe (Referenced Schema)
  -> FilePath
  -> proxy (Verb method status cs (Headers hs a))
  -> Swagger
mkEndpointWithSchemaRef mref path _ = mempty
  & paths.at path ?~
    (mempty & method ?~ (mempty
      & at code ?~ Inline (mempty
            & content .~ InsOrdHashMap.fromList
              [(t, mempty & schema .~ mref) | t <- responseContentTypes]
            & headers .~ responseHeaders)))
  where
    method               = swaggerMethod (Proxy :: Proxy method)
    code                 = fromIntegral (natVal (Proxy :: Proxy status))
    responseContentTypes = allContentType (Proxy :: Proxy cs)
    responseHeaders      = Inline <$> toAllResponseHeaders (Proxy :: Proxy hs)

mkEndpointNoContentVerb :: forall proxy method.
  (SwaggerMethod method)
  => FilePath                      -- ^ Endpoint path.
  -> proxy (NoContentVerb method)  -- ^ Method
  -> Swagger
mkEndpointNoContentVerb path _ = mempty
  & paths.at path ?~
    (mempty & method ?~ (mempty
      & at code ?~ Inline mempty))
  where
    method               = swaggerMethod (Proxy :: Proxy method)
    code                 = 204 -- hardcoded in servant-server

-- | Add parameter to every operation in the spec.
addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

-- | Add RequestBody to every operations in the spec.
addRequestBody :: RequestBody -> Swagger -> Swagger
addRequestBody rb = allOperations . requestBody ?~ Inline rb

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

addDefaultResponse404 :: ParamName -> Swagger -> Swagger
addDefaultResponse404 pname = setResponseWith (\old _new -> alter404 old) 404 (return response404)
  where
    sname = markdownCode pname
    description404 = sname <> " not found"
    alter404 = description %~ ((sname <> " or ") <>)
    response404 = mempty & description .~ description404

addDefaultResponse400 :: ParamName -> Swagger -> Swagger
addDefaultResponse400 pname = setResponseWith (\old _new -> alter400 old) 400 (return response400)
  where
    sname = markdownCode pname
    description400 = "Invalid " <> sname
    alter400 = description %~ (<> (" or " <> sname))
    response400 = mempty & description .~ description400

-- | Methods, available for Swagger.
class SwaggerMethod method where
  swaggerMethod :: proxy method -> Lens' PathItem (Maybe Operation)

instance SwaggerMethod 'GET     where swaggerMethod _ = get
instance SwaggerMethod 'PUT     where swaggerMethod _ = put
instance SwaggerMethod 'POST    where swaggerMethod _ = post
instance SwaggerMethod 'DELETE  where swaggerMethod _ = delete
instance SwaggerMethod 'OPTIONS where swaggerMethod _ = options
instance SwaggerMethod 'HEAD    where swaggerMethod _ = head_
instance SwaggerMethod 'PATCH   where swaggerMethod _ = patch

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs, KnownNat status, SwaggerMethod method) => HasSwagger (Verb method status cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Verb method status cs (Headers '[] a)))

-- | @since 1.1.7
instance (ToSchema a, Accept ct, KnownNat status, SwaggerMethod method) => HasSwagger (Stream method status fr ct a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Verb method status '[ct] (Headers '[] a)))

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs, AllToResponseHeader hs, KnownNat status, SwaggerMethod method)
  => HasSwagger (Verb method status cs (Headers hs a)) where
  toSwagger = mkEndpoint "/"

-- ATTENTION: do not remove this instance!
-- A similar instance above will always use the more general
-- polymorphic -- HasSwagger instance and will result in a type error
-- since 'NoContent' does not have a 'ToSchema' instance.
instance (AllAccept cs, KnownNat status, SwaggerMethod method) => HasSwagger (Verb method status cs NoContent) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Verb method status cs (Headers '[] NoContent)))

instance (AllAccept cs, AllToResponseHeader hs, KnownNat status, SwaggerMethod method)
  => HasSwagger (Verb method status cs (Headers hs NoContent)) where
  toSwagger = mkEndpointNoContent "/"

instance (SwaggerMethod method) => HasSwagger (NoContentVerb method) where
  toSwagger =  mkEndpointNoContentVerb "/"

instance (HasSwagger a, HasSwagger b) => HasSwagger (a :<|> b) where
  toSwagger _ = toSwagger (Proxy :: Proxy a) <> toSwagger (Proxy :: Proxy b)

-- | @'Vault'@ combinator does not change our specification at all.
instance (HasSwagger sub) => HasSwagger (Vault :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

-- | @'IsSecure'@ combinator does not change our specification at all.
instance (HasSwagger sub) => HasSwagger (IsSecure :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

-- | @'RemoteHost'@ combinator does not change our specification at all.
instance (HasSwagger sub) => HasSwagger (RemoteHost :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

-- | @'HttpVersion'@ combinator does not change our specification at all.
instance (HasSwagger sub) => HasSwagger (HttpVersion :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

-- | @'WithNamedContext'@ combinator does not change our specification at all.
instance (HasSwagger sub) => HasSwagger (WithNamedContext x c sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (sym :> sub) where
  toSwagger _ = prependPath piece (toSwagger (Proxy :: Proxy sub))
    where
      piece = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub, KnownSymbol (FoldDescription mods)) => HasSwagger (Capture' mods sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & prependPath capture
    & addDefaultResponse404 tname
    where
      pname = symbolVal (Proxy :: Proxy sym)
      tname = Text.pack pname
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      capture = "{" <> pname <> "}"
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ True
        & in_ .~ ParamPath
        & schema ?~ Inline (mempty
            & paramSchema .~ toParamSchema (Proxy :: Proxy a))

-- | Swagger Spec doesn't have a notion of CaptureAll, this instance is the best effort.
instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (CaptureAll sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Capture sym a :> sub))

instance (KnownSymbol desc, HasSwagger api) => HasSwagger (Description desc :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & allOperations.description %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

instance (KnownSymbol desc, HasSwagger api) => HasSwagger (Summary desc :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & allOperations.summary %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub, SBoolI (FoldRequired mods), KnownSymbol (FoldDescription mods)) => HasSwagger (QueryParam' mods sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & in_ .~ ParamQuery
        & schema ?~ Inline sch
      sch = mempty
        & paramSchema .~ toParamSchema (Proxy :: Proxy a)

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (QueryParams sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      param = mempty
        & name .~ tname
        & in_ .~ ParamQuery
        & schema ?~ Inline sch
      sch = mempty
        & paramSchema .~ pschema
      pschema = mempty
#if MIN_VERSION_swagger2(2,4,0)
        & type_ ?~ SwaggerArray
#else
        & type_ .~ SwaggerArray
#endif
        & items ?~ SwaggerItemsObject (Inline $ mempty & paramSchema .~ toParamSchema (Proxy :: Proxy a))

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (QueryFlag sym :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      param = mempty
        & name .~ tname
        & in_ .~ ParamQuery
        & allowEmptyValue ?~ True
        & schema ?~ (Inline $ mempty
            & paramSchema .~ (toParamSchema (Proxy :: Proxy Bool)
                & default_ ?~ toJSON False))

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub, SBoolI (FoldRequired mods), KnownSymbol (FoldDescription mods)) => HasSwagger (Header' mods  sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & in_ .~ ParamHeader
        & schema ?~ (Inline $ mempty
            & paramSchema .~ toParamSchema (Proxy :: Proxy a))

instance (ToSchema a, AllAccept cs, HasSwagger sub, KnownSymbol (FoldDescription mods)) => HasSwagger (ReqBody' mods cs a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addRequestBody reqBody
    & addDefaultResponse400 tname
    & components.schemas %~ (<> defs)
    where
      tname = "body"
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
      reqBody = (mempty :: RequestBody)
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- allContentType (Proxy :: Proxy cs)]

-- | This instance is an approximation.
--
-- @since 1.1.7
instance (ToSchema a, Accept ct, HasSwagger sub, KnownSymbol (FoldDescription mods)) => HasSwagger (StreamBody' mods fr ct a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addRequestBody reqBody
    & addDefaultResponse400 tname
    & components.schemas %~ (<> defs)
    where
      tname = "body"
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
      reqBody = (mempty :: RequestBody)
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- toList $ contentTypes (Proxy :: Proxy ct)]

-- =======================================================================
-- Below are the definitions that should be in Servant.API.ContentTypes
-- =======================================================================

class AllAccept cs where
  allContentType :: Proxy cs -> [MediaType]

instance AllAccept '[] where
  allContentType _ = []

instance (Accept c, AllAccept cs) => AllAccept (c ': cs) where
  allContentType _ = contentType (Proxy :: Proxy c) : allContentType (Proxy :: Proxy cs)

class ToResponseHeader h where
  toResponseHeader :: Proxy h -> (HeaderName, Swagger.Header)

instance (KnownSymbol sym, ToParamSchema a) => ToResponseHeader (Header sym a) where
  toResponseHeader _ = (hname, Swagger.Header Nothing hschema)
    where
      hname = Text.pack (symbolVal (Proxy :: Proxy sym))
      hschema = Just $ Inline $ mempty & paramSchema .~ toParamSchema (Proxy :: Proxy a)

class AllToResponseHeader hs where
  toAllResponseHeaders :: Proxy hs -> InsOrdHashMap HeaderName Swagger.Header

instance AllToResponseHeader '[] where
  toAllResponseHeaders _ = mempty

instance (ToResponseHeader h, AllToResponseHeader hs) => AllToResponseHeader (h ': hs) where
  toAllResponseHeaders _ = InsOrdHashMap.insert headerName headerBS hdrs
    where
      (headerName, headerBS) = toResponseHeader (Proxy :: Proxy h)
      hdrs = toAllResponseHeaders (Proxy :: Proxy hs)

instance AllToResponseHeader hs => AllToResponseHeader (HList hs) where
  toAllResponseHeaders _ = toAllResponseHeaders (Proxy :: Proxy hs)
