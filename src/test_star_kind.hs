    {-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

    import GHC.TypeLits
    import Data.Kind(Type)

    main = undefined

    class Sized a where
      type SizeOf a :: Nat

    instance Sized () where
      type SizeOf () = 0

    instance (Sized a, Sized b) => Sized (a, b) where
      type SizeOf (a, b) = SizeOf a + SizeOf b

    data Vec :: Nat -> Type -> Type where
      Nil  :: Vec 0 a
      Cons :: a -> Vec n a -> Vec (n + 1) a

    instance Sized a => Sized (Vec n a) where
      type SizeOf (Vec n a) = n * SizeOf a
