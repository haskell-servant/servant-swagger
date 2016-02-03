{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Swagger.Internal.TypeLevel.Every where

import Control.Applicative
import Data.Functor.Contravariant (Op(..))
import Data.Proxy
import GHC.Exts (Constraint)

import Servant.Swagger.Internal.TypeLevel.TMap

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XGADTs
-- >>> :set -XRankNTypes
-- >>> :set -XScopedTypeVariables
-- >>> import GHC.TypeLits
-- >>> import Data.List

-- | Apply multiple constraint constructors to a type.
--
-- @
-- EveryTF '[Show, Read] a ~ (Show a, Read a)
-- @
--
-- Note that since this is a type family, you have to alway fully apply @'EveryTF'@.
--
-- For partial application of multiple constraint constructors see @'Every'@.
type family EveryTF cs x :: Constraint where
  EveryTF '[] x = ()
  EveryTF (c ': cs) x = (c x, EveryTF cs x)

-- | @'EveryProof' cs x@ is a proof that @x@ satisfies all constraints in @cs@.
--
-- E.g. a value of type @'EveryProof' '[Show, Read] a@ proves that @a@ has both @Show@ and @Read@ instances.
--
-- Pattern matching on @'EveryProofCons'@ brings one constraint at a time to the scope.
data EveryProof cs x where
  EveryProofNil  :: EveryProof '[] x
  EveryProofCons :: c x => EveryProof cs x -> EveryProof (c ': cs) x

-- | Apply multiple constraint constructors to a type as a class.
--
-- This is different from @'EveryTF'@ in that it allows partial application.
-- However, @'Every' cs@ does not immediately bring @'EveryTF'@ into scope, so you
-- can't use the instances right away:
--
-- >>> :{
-- let f :: Every [Show, Num] a => a -> String
--     f = show
-- :}
-- ...
--     Could not deduce (Show a) arising from a use of ‘show’
--     from the context (Every '[Show, Num] a)
-- ...
--
-- To bring individual constraints into scope use @'every'@ and pattern match on the proof:
--
-- >>> :{
-- let f :: forall a. Every [Show, Num] a => a -> String
--     f = case (every :: EveryProof [Show, Num] a) of EveryProofCons (EveryProofCons EveryProofNil) -> show
-- in f 123
-- :}
-- "123"
--
-- Alternatively, you can use @'appWith'@ to convert @'Every' cs@ constraint into @'EveryTF' cs@.
-- @'appWith' pqs pcs f px@ applies a function @f@ with regular constraints to a proxied type @px@,
-- using @'Every' cs x@ constraint of @px@. It automatically proves that @px@ satisfies the
-- regular constraints of @f@.
--
-- >>> let num = Proxy :: Proxy '[Num]
-- >>> let enumOrd = Proxy :: Proxy '[Enum, Ord]
-- >>> let xs :: (Num a, Enum a, Ord a) => p a -> [a]; xs _ = sort [1, 4, 3, 2]
-- >>> let ys :: (Num a, Every '[Enum, Ord] a) => p a -> [a]; ys = appWith num enumOrd xs
-- >>> ys (Proxy :: Proxy Int)
-- [1,2,3,4]
--
-- It might be useful to look at the type of @'appWith'@ also like this:
--
-- @
-- appWith :: pq qs -> p cs
--    -> (forall x p'. (EveryTF qs x, EveryTF cs x) => p' x -> f x)
--    -> (forall x p'. (EveryTF qs x, Every   cs x) => p' x -> f x)
-- @
class Every (cs :: [* -> Constraint]) (x :: *) where
  every :: EveryProof cs x
  appWith :: EveryTF qs x => pq qs -> p cs -> (forall y p'. (EveryTF qs y, EveryTF cs y) => p' y -> f y) -> p'' x -> f x

instance Every '[] x where
  every = EveryProofNil
  appWith _ _ f x = f x

instance (c x, Every cs x) => Every (c ': cs) x where
  every = EveryProofCons every

  appWith :: forall qs pq p p'' f. EveryTF qs x => pq qs -> p (c ': cs) -> (forall y p'. (EveryTF (c ': cs) y, EveryTF qs y) => p' y -> f y) -> p'' x -> f x
  appWith _ _ f px = case (every :: EveryProof (c ': cs) x) of
    EveryProofCons _ -> appWith (Proxy :: Proxy (c ': qs)) (Proxy :: Proxy cs) f px

-- | Convert some constraints from @'Every'@ to @'EveryTF'@
-- for a function which result type does not depend on its argument's type.
appWith_ :: (EveryTF qs x, Every cs x) => pq qs -> p cs -> (forall y p'. (EveryTF qs y, EveryTF cs y) => p' y -> a) -> p'' x -> a
appWith_ q p f = getConst . appWith q p (\p' -> Const (f p'))

-- | Convert all constraints from @'Every'@ to @'EveryTF'@.
app :: Every cs x => p cs -> (forall y p'. EveryTF cs y => p' y -> f y) -> p'' x -> f x
app = appWith (Proxy :: Proxy '[])

-- | Convert all constraints from @'Every'@ to @'EveryTF'@
-- for a function which result type does not depend on its argument's type.
app_ :: Every cs x => p cs -> (forall y p'. EveryTF cs y => p' y -> a) -> p'' x -> a
app_ p f = getConst . app p (\p' -> Const (f p'))

-- | Convert all constraints from @'Every'@ to @'EveryTF'@
-- for a term-level function which result type does not depend on its argument's type.
appVal_ :: forall p a cs x. Every cs x => p cs -> (forall y. EveryTF cs y => y -> a) -> x -> a
appVal_ p f = getOp (app p f' (Proxy :: Proxy x))
  where
    f' :: forall p' y. EveryTF cs y => p' y -> Op a y
    f' _ = Op f

-- | Like @'tmap'@, but uses @'Every'@ for multiple constraints.
--
-- >>> let zero :: forall p a. (Show a, Num a) => p a -> String; zero _ = show (0 :: a)
-- >>> tmapEvery (Proxy :: Proxy [Show, Num]) zero (Proxy :: Proxy [Int, Float])
-- ["0","0.0"]
tmapEvery :: forall a cs p p'' xs. (TMap (Every cs) xs) =>
  p cs -> (forall x p'. EveryTF cs x => p' x -> a) -> p'' xs -> [a]
tmapEvery pcs f = tmap (Proxy :: Proxy (Every cs)) (app_ pcs f)

