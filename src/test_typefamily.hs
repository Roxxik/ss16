{-# LANGUAGE TypeInType, GADTs, TypeFamilies, TypeApplications, RankNTypes,
  ScopedTypeVariables #-}

import GHC.TypeLits
import Data.Kind
import Data.Proxy

main = undefined

data A :: Type -> Type where
  A :: Int -> A a

class Sized a where
  type SizeOf a :: Nat

instance Sized () where
  type SizeOf () = 0

type family Size a :: Nat where
  Size ()    = 0

size :: forall a n. (Sized a, KnownNat n, SizeOf a ~ n) => A a -> Integer
size a = natVal (Proxy @n)

size' :: forall a n. (KnownNat n, Size a ~ n) => A a -> Integer
size' a = natVal (Proxy @n)
