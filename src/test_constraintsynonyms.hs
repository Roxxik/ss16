{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, ScopedTypeVariables, FlexibleContexts #-}

import GHC.TypeLits
import Data.Proxy

main = undefined

class Sized a where
  type SizeOf a :: Nat

type Sized1 a size = (Sized a, KnownNat size, SizeOf a ~ size)

type Sized2 a size = (Sized a, KnownNat size)

type Sized3 a = (Sized a, KnownNat (SizeOf a))

data A a = A

size1 :: forall a size. (Sized a, KnownNat size, SizeOf a ~ size) => A a -> Integer
size1 _ = natVal (Proxy :: Proxy size)

size2 :: forall a size. (Sized1 a size) => A a -> Integer
size2 _ = natVal (Proxy :: Proxy size)

size3 :: forall a size. (Sized2 a size, SizeOf a ~ size) => A a -> Integer
size3 _ = natVal (Proxy :: Proxy size)

size4 :: forall a. (Sized a, KnownNat (SizeOf a)) => A a -> Integer
size4 _ = natVal (Proxy :: Proxy (SizeOf a))

size5 :: forall a. (Sized3 a) => A a -> Integer
size5 _ = natVal (Proxy :: Proxy (SizeOf a))
