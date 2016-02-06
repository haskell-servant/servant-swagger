{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Swagger.Internal.TypeLevel.API where

import Data.Type.Bool (If)
import Servant.API
import GHC.Exts (Constraint)

-- | Build a list of endpoints from an API.
type family EndpointsList api where
  EndpointsList (a :<|> b) = AppendList (EndpointsList a) (EndpointsList b)
  EndpointsList (e :> a)   = MapSub e (EndpointsList a)
  EndpointsList a = '[a]

-- | Check whether @sub@ is a sub API of @api@.
type family IsSubAPI sub api :: Constraint where
  IsSubAPI sub api = AllIsElem (EndpointsList sub) api

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

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b = ()
  Or a () = ()

type family IsIn sub api :: Constraint where
  IsIn e (a :<|> b) = Or (IsIn e a) (IsIn e b)
  IsIn (e :> a) (e :> b) = IsIn a b
  IsIn e e = ()

-- | Check whether a type is a member of a list of types.
-- This is a type-level analogue of @'elem'@.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- | @'AddBodyType' c cs a as@ adds type @a@ to the list @as@
-- only if @c@ is in @cs@ and @a@ is not in @as@.
-- This allows to build a list of unique body types.
type AddBodyType c cs a as = If (Elem c cs) (Insert a as) as

-- | Insert type @x@ into a type list @xs@ only if it is not already there.
type Insert x xs = If (Elem x xs) xs (x ': xs)

-- | Merge two lists, ignoring any type in @xs@ which occurs also in @ys@.
type family Merge xs ys where
  Merge '[] ys = ys
  Merge (x ': xs) ys = Insert x (Merge xs ys)

-- | Extract a list of unique "body" types for a specific content-type from a servant API.
type family BodyTypes c api :: [*] where
  BodyTypes c (Delete  cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes c (Get     cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes c (Patch   cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes c (Post    cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes c (Put     cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes c (Delete  cs a) = AddBodyType c cs a '[]
  BodyTypes c (Get     cs a) = AddBodyType c cs a '[]
  BodyTypes c (Patch   cs a) = AddBodyType c cs a '[]
  BodyTypes c (Post    cs a) = AddBodyType c cs a '[]
  BodyTypes c (Put     cs a) = AddBodyType c cs a '[]
  BodyTypes c (ReqBody cs a :> api) = AddBodyType c cs a (BodyTypes c api)
  BodyTypes c (e :> api) = BodyTypes c api
  BodyTypes c (a :<|> b) = Merge (BodyTypes c a) (BodyTypes c b)
  BodyTypes c api = '[]

