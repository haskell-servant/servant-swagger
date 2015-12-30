{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Swagger.Internal where

import Control.Arrow (first)
import Control.Lens
import Data.Aeson
import Data.Data.Lens (template)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (dropWhileEnd)
import Data.Monoid
import Data.Proxy
import qualified Data.Swagger as Swagger
import Data.Swagger hiding (Header)
import Data.Swagger.Declare
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits
import GHC.Exts
import Network.HTTP.Media (MediaType)
import Servant.API

class HasSwagger api where
  toSwagger :: Proxy api -> Swagger

instance HasSwagger Raw where
  toSwagger _ = mempty & paths.pathsMap.at "/" ?~ mempty

-- | All operations of sub API.
subOperations :: forall sub api. (IsSubAPI sub api, HasSwagger sub) =>
  Proxy sub -> Proxy api -> Traversal' Swagger Operation
subOperations sub _ = paths.pathsMap.itraversed.indices (`elem` ps).template
  where
    ps = toSwagger sub ^. paths.pathsMap.to HashMap.keys

-- | Tag an operation.
addTag :: TagName -> Operation -> Operation
addTag tag = operationTags %~ (tag:)

-- | Set a response for an operation.
setResponse :: HttpStatusCode -> Response -> Operation -> Operation
setResponse code res = operationResponses.responsesResponses.at code ?~ Inline res

(</>) :: FilePath -> FilePath -> FilePath
x </> y = case trim y of
  "" -> "/" <> trim x
  y' -> "/" <> trim x <> "/" <> y'
  where
    trim = dropWhile (== '/') . dropWhileEnd (== '/')

mkEndpoint :: forall a cs hs proxy _verb. (ToSchema a, AllAccept cs, AllToResponseHeader hs)
  => FilePath
  -> Lens' PathItem (Maybe Operation)
  -> HttpStatusCode
  -> proxy (_verb cs (Headers hs a))
  -> Swagger
mkEndpoint path verb code proxy
  = mkEndpointWithSchemaRef (Just ref) path verb code proxy
      & definitions .~ defs
  where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty

noContentEndpoint :: forall cs proxy verb. (AllAccept cs)
  => FilePath
  -> Lens' PathItem (Maybe Operation)
  -> proxy (verb cs ())
  -> Swagger
noContentEndpoint path verb _ = mkEndpointWithSchemaRef Nothing path verb 204 (Proxy :: Proxy (verb cs (Headers '[] ())))

mkEndpointWithSchemaRef :: forall cs hs proxy verb a. (AllAccept cs, AllToResponseHeader hs)
  => Maybe (Referenced Schema)
  -> FilePath
  -> Lens' PathItem (Maybe Operation)
  -> HttpStatusCode
  -> proxy (verb cs (Headers hs a))
  -> Swagger
mkEndpointWithSchemaRef mref path verb code _ = mempty
  & paths.pathsMap.at path ?~
    (mempty & verb ?~ (mempty
      & operationProduces ?~ MimeList (allContentType (Proxy :: Proxy cs))
      & operationResponses .~ (mempty
        & responsesResponses . at code ?~ Inline (mempty
            & responseSchema  .~ mref
            & responseHeaders .~ toAllResponseHeaders (Proxy :: Proxy hs)))))

-- | Prepend path to all API endpoints.
prependPath :: FilePath -> Swagger -> Swagger
prependPath path spec = spec & paths.pathsMap %~ f
  where
    f = HashMap.fromList . map (first (path </>)) . HashMap.toList

-- | Add parameter to every operation in the spec.
addParam :: Param -> Swagger -> Swagger
addParam param spec = spec & template.operationParameters %~ (Inline param :)

-- | Add accepted content types to every operation in the spec.
addConsumes :: [MediaType] -> Swagger -> Swagger
addConsumes cs spec = spec & template.operationConsumes %~ (<> Just (MimeList cs))

-- | Add/modify response for every operation in the spec.
addResponseWith :: (Response -> Response -> Response) -> HttpStatusCode -> Response -> Swagger -> Swagger
addResponseWith f code new spec = spec
  & paths.template.responsesResponses . at code %~ Just . Inline . combine
  where
    combine (Just (Ref (Reference name))) = case spec ^. responses.at name of
      Just old -> f old new
      Nothing  -> new -- FIXME: what is the right choice here?
    combine (Just (Inline old)) = f old new
    combine Nothing = new

-- | Add/overwrite response for every operation in the spec.
addResponse :: HttpStatusCode -> Response -> Swagger -> Swagger
addResponse = addResponseWith (\_old new -> new)

addDefaultResponse404 :: ParamName -> Swagger -> Swagger
addDefaultResponse404 name = addResponseWith (\old _new -> alter404 old) 404 response404
  where
    description404 = name <> " not found"
    alter404 = description %~ ((name <> " or ") <>)
    response404 = mempty & description .~ description404

addDefaultResponse400 :: ParamName -> Swagger -> Swagger
addDefaultResponse400 name = addResponseWith (\old _new -> alter400 old) 400 response400
  where
    description400 = "Invalid " <> name
    alter400 = description %~ (<> (" or " <> name))
    response400 = mempty & description .~ description400

-- -----------------------------------------------------------------------
-- DELETE
-- -----------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Delete cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Delete cs (Headers '[] a)))

instance (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Delete cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemDelete 200

instance AllAccept cs => HasSwagger (Delete cs ()) where
  toSwagger = noContentEndpoint "/" pathItemDelete

-- -----------------------------------------------------------------------
-- GET
-- -----------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Get cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Get cs (Headers '[] a)))

instance (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Get cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemGet 200

instance AllAccept cs => HasSwagger (Get cs ()) where
  toSwagger = noContentEndpoint "/" pathItemGet

-- -----------------------------------------------------------------------
-- PATCH
-- -----------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Patch cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Patch cs (Headers '[] a)))

instance (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Patch cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemPatch 200

instance AllAccept cs => HasSwagger (Patch cs ()) where
  toSwagger = noContentEndpoint "/" pathItemPatch

-- -----------------------------------------------------------------------
-- PUT
-- -----------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Put cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Put cs (Headers '[] a)))

instance (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Put cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemPut 200

instance AllAccept cs => HasSwagger (Put cs ()) where
  toSwagger = noContentEndpoint "/" pathItemPut

-- -----------------------------------------------------------------------
-- POST
-- -----------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Post cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Post cs (Headers '[] a)))

instance (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Post cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemPost 201

instance AllAccept cs => HasSwagger (Post cs ()) where
  toSwagger = noContentEndpoint "/" pathItemPost


instance (HasSwagger a, HasSwagger b) => HasSwagger (a :<|> b) where
  toSwagger _ = toSwagger (Proxy :: Proxy a) <> toSwagger (Proxy :: Proxy b)

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (sym :> sub) where
  toSwagger _ = prependPath piece (toSwagger (Proxy :: Proxy sub))
    where
      piece = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (Capture sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & prependPath capture
    & addDefaultResponse404 (Text.pack name)
    where
      name = symbolVal (Proxy :: Proxy sym)
      capture = "{" <> name <> "}"
      param = mempty
        & paramName .~ Text.pack name
        & paramRequired ?~ True
        & paramSchema .~ ParamOther (mempty
            & paramOtherSchemaIn .~ ParamPath
            & parameterSchema .~ toParamSchema (Proxy :: Proxy a))

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (QueryParam sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 (Text.pack name)
    where
      name = symbolVal (Proxy :: Proxy sym)
      param = mempty
        & paramName .~ Text.pack name
        & paramSchema .~ ParamOther (mempty
            & paramOtherSchemaIn .~ ParamQuery
            & parameterSchema .~ toParamSchema (Proxy :: Proxy a))

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (QueryParams sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 (Text.pack name)
    where
      name = symbolVal (Proxy :: Proxy sym)
      param = mempty
        & paramName .~ Text.pack name
        & paramSchema .~ ParamOther (mempty
            & paramOtherSchemaIn .~ ParamQuery
            & parameterSchema .~ (mempty
                & schemaType .~ SwaggerArray
                & schemaItems ?~ SwaggerItemsPrimitive (Just CollectionMulti) (toParamSchema (Proxy :: Proxy a))))

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (QueryFlag sym :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 (Text.pack name)
    where
      name = symbolVal (Proxy :: Proxy sym)
      param = mempty
        & paramName .~ Text.pack name
        & paramSchema .~ ParamOther (mempty
            & paramOtherSchemaIn .~ ParamQuery
            & paramOtherSchemaAllowEmptyValue ?~ True
            & parameterSchema .~ (toParamSchema (Proxy :: Proxy Bool)
                & schemaDefault ?~ toJSON False))

instance (KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (Header sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 (Text.pack name)
    where
      name = symbolVal (Proxy :: Proxy sym)
      param = mempty
        & paramName .~ Text.pack name
        & paramSchema .~ ParamOther (mempty
            & paramOtherSchemaIn .~ ParamHeader
            & parameterSchema .~ toParamSchema (Proxy :: Proxy Bool))

instance (ToSchema a, AllAccept cs, HasSwagger sub) => HasSwagger (ReqBody cs a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam param
    & addConsumes (allContentType (Proxy :: Proxy cs))
    & addDefaultResponse400 name
    & definitions %~ (<> defs)
    where
      name = "body"
      (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
      param = mempty
        & paramName   .~ "body"
        & paramSchema .~ ParamBody ref

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
  toResponseHeader _ = (hname, Swagger.Header Nothing schema)
    where
      hname = Text.pack (symbolVal (Proxy :: Proxy sym))
      schema = toParamSchema (Proxy :: Proxy a)

class AllToResponseHeader hs where
  toAllResponseHeaders :: Proxy hs -> HashMap HeaderName Swagger.Header

instance AllToResponseHeader '[] where
  toAllResponseHeaders _ = mempty

instance (ToResponseHeader h, AllToResponseHeader hs) => AllToResponseHeader (h ': hs) where
  toAllResponseHeaders _ = HashMap.insert name header headers
    where
      (name, header) = toResponseHeader (Proxy :: Proxy h)
      headers = toAllResponseHeaders (Proxy :: Proxy hs)

instance AllToResponseHeader hs => AllToResponseHeader (HList hs) where
  toAllResponseHeaders _ = toAllResponseHeaders (Proxy :: Proxy hs)

-- | Check that every element of @xs@ is an endpoint of @api@.
type family AllIsElem xs api :: Constraint where
  AllIsElem '[] api = ()
  AllIsElem (x ': xs) api = (IsIn x api, AllIsElem xs api)

-- | Apply @(e :>)@ to every API in @xs@.
type family MapSub e xs where
  MapSub e '[] = '[]
  MapSub e (x ': xs) = (e :> x) ': MapSub e xs

-- | Append two type-level lists.
type family AppendList xs ys where
  AppendList '[]       ys = ys
  AppendList (x ': xs) ys = x ': AppendList xs ys

-- | Build a list of endpoints from an API.
type family EndpointsList api where
  EndpointsList (a :<|> b) = AppendList (EndpointsList a) (EndpointsList b)
  EndpointsList (e :> a)   = MapSub e (EndpointsList a)
  EndpointsList a = '[a]

-- | Check whether @sub@ is a sub API of @api@.
type family IsSubAPI sub api :: Constraint where
  IsSubAPI sub api = AllIsElem (EndpointsList sub) api

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b = ()
  Or a () = ()

type family IsIn sub api :: Constraint where
  IsIn e (a :<|> b) = Or (IsIn e a) (IsIn e b)
  IsIn (e :> a) (e :> b) = IsIn a b
  IsIn e e = ()

