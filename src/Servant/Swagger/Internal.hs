{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Swagger.Internal where

import Control.Lens
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import qualified Data.Swagger as Swagger
import Data.Swagger hiding (Header)
import Data.Swagger.Declare
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits
import Network.HTTP.Media (MediaType)
import Servant.API

class HasSwagger api where
  toSwagger :: Proxy api -> Swagger

instance HasSwagger Raw where toSwagger _ = mempty

mkEndpoint :: forall a cs hs proxy _verb. (ToSchema a, AllAccept cs, AllToResponseHeader hs)
  => FilePath
  -> Lens' PathItem (Maybe Operation)
  -> Int
  -> proxy (_verb cs (Headers hs a))
  -> Swagger
mkEndpoint path verb code _ = mempty
  & definitions .~ defs
  & paths.pathsMap.at path ?~
    (mempty & verb ?~ (mempty
      & operationProduces ?~ MimeList (allContentType (Proxy :: Proxy cs))
      & operationResponses .~ (mempty
        & responsesResponses . at code ?~ Inline (mempty
            & responseSchema ?~ aref
            & responseHeaders .~ toAllResponseHeaders (Proxy :: Proxy hs)))))
  where
    (defs, aref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs) => HasSwagger (Post cs a) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Post cs (Headers '[] a)))

instance {-# OVERLAPPING #-} (ToSchema a, AllAccept cs, AllToResponseHeader hs) => HasSwagger (Post cs (Headers hs a)) where
  toSwagger = mkEndpoint "/" pathItemPost 201

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
  toResponseHeader :: Proxy h -> (Text, Swagger.Header)

instance (KnownSymbol sym, ToParamSchema a) => ToResponseHeader (Header sym a) where
  toResponseHeader _ = (hname, Swagger.Header Nothing Nothing schema)
    where
      hname = Text.pack (symbolVal (Proxy :: Proxy sym))
      schema = toParamSchema (Proxy :: Proxy a)

class AllToResponseHeader hs where
  toAllResponseHeaders :: Proxy hs -> HashMap Text Swagger.Header

instance AllToResponseHeader '[] where
  toAllResponseHeaders _ = mempty

instance (ToResponseHeader h, AllToResponseHeader hs) => AllToResponseHeader (h ': hs) where
  toAllResponseHeaders _ = HashMap.insert name header headers
    where
      (name, header) = toResponseHeader (Proxy :: Proxy h)
      headers = toAllResponseHeaders (Proxy :: Proxy hs)

