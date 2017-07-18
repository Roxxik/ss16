{-# LANGUAGE TypeFamilyDependencies #-}

data Nat = Z | S Nat

type family Plus (a :: Nat) (b :: Nat) = (sum :: Nat) | sum a -> b, sum b -> a where
  Plus Z b = b
  Plus a Z = a
  Plus (S a) b = Plus a (S b)
