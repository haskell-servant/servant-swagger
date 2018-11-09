{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Swagger.Internal.Orphans where

import           Data.Proxy
                 (Proxy (..))
import           Data.Swagger
import           Servant.Types.SourceT
                 (SourceT)

-- | Pretend that 'SourceT m a' is '[a]'.
--
-- @since 1.1.7
--
instance ToSchema a => ToSchema (SourceT m a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
