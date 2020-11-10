{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Servant.Swagger.Internal.Orphans where

import           Data.Proxy
                 (Proxy (..))
import           Data.Swagger
import           Servant.Types.SourceT
                 (SourceT)
import           Servant.API (WithStatus(..))

-- | Pretend that 'SourceT m a' is '[a]'.
--
-- @since 1.1.7
--
instance ToSchema a => ToSchema (SourceT m a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])

-- @since <TODO>
#if __GLASGOW_HASKELL__ >= 881
deriving instance ToSchema a => ToSchema (WithStatus s a)
#endif
