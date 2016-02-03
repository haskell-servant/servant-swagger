{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Swagger.Internal.Test where

import Data.Aeson (ToJSON)
import Data.Swagger
import Data.Swagger.Schema.Validation
import Data.Typeable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)

import Servant.API
import Servant.Swagger.Internal.TypeLevel

-- $setup
-- >>> import GHC.Generics
-- >>> import Test.QuickCheck
-- >>> :set -XDeriveGeneric
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators

-- | Verify that every type used with @'JSON'@ content type in a servant API
-- has compatible @'ToJSON'@ and @'ToSchema'@ instances using @'validateToJSON'@.
--
-- >>> data User = User { name :: String, age :: Maybe Int } deriving (Show, Generic, Typeable)
-- >>> newtype UserId = UserId String deriving (Show, Generic, Typeable, ToJSON, Arbitrary)
-- >>> instance ToJSON User
-- >>> instance ToSchema User
-- >>> instance ToSchema UserId
-- >>> instance Arbitrary User where arbitrary = User <$> arbitrary <*> arbitrary
-- >>> type MyAPI = (Capture "user_id" UserId :> Get '[JSON] User) :<|> (ReqBody '[JSON] User :> Post '[JSON] UserId)
-- >>> hspec $ context "ToJSON matches ToSchema" $ validateEveryToJSON (Proxy :: Proxy MyAPI)
-- <BLANKLINE>
-- ToJSON matches ToSchema
--   User
--   User
--   UserId
-- <BLANKLINE>
-- Finished in ... seconds
-- 3 examples, 0 failures
validateEveryToJSON :: forall proxy api. TMap (Every [Typeable, Show, Arbitrary, ToJSON, ToSchema]) (BodyTypes JSON api) => proxy api -> Spec
validateEveryToJSON _ = props (Proxy :: Proxy [ToJSON, ToSchema]) (\x -> validateToJSON x == []) (Proxy :: Proxy (BodyTypes JSON api))

-- * QuickCheck-related stuff

-- | Construct property tests for each type in a list.
-- The name for each property is the name of the corresponding type.
--
-- >>> :{
--  hspec $
--    context "read . show == id" $
--      props
--        (Proxy :: Proxy [Eq, Show, Read])
--        (\x -> read (show x) == x)
--        (Proxy :: Proxy [Bool, Int, String])
-- :}
-- <BLANKLINE>
-- read . show == id
--   Bool
--   Int
--   [Char]
-- <BLANKLINE>
-- Finished in ... seconds
-- 3 examples, 0 failures
props :: forall p p'' cs xs. TMap (Every (Typeable ': Show ': Arbitrary ': cs)) xs =>
  p cs -> (forall x. EveryTF cs x => x -> Bool) -> p'' xs -> Spec
props _ f px = sequence_ specs
  where
    specs :: [Spec]
    specs = tmapEvery (Proxy :: Proxy (Typeable ': Show ': Arbitrary ': cs)) aprop px

    aprop :: forall p' a. (EveryTF cs a, Typeable a, Show a, Arbitrary a) => p' a -> Spec
    aprop _ = prop (show (typeOf (undefined :: a))) (f :: a -> Bool)

