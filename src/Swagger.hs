{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Swagger where
import Data.Char
import           Control.Lens
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Exts (Constraint)
import           GHC.TypeLits
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Swagger.Internal
import           Servant.Utils.Links
