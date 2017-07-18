{-# LANGUAGE RankNTypes, KindSignatures, DataKinds, ExistentialQuantification, GADTs, TypeApplications #-}

import GHC.TypeLits
import Data.Kind

main = undefined

data A a = A
data B = B { unB :: forall a. Eq a => A a }

a :: Eq a => A a
a = A

b :: B
b = B a

test :: A Bool
test = A

test' :: B
test' = B test

data B' :: Type where
  B' :: Eq a => A a -> B'

b' :: B'
b' = B' a

data DA (a :: Nat) = DA
data DB = DB { unDB :: forall (a :: Nat). KnownNat a => DA a }



da :: KnownNat a => DA a
da = DA

db :: DB
db = DB da

testd :: DA 5
testd = DA

testd' :: DB
testd' = DB testd



data DB' :: Type where
  DB' :: KnownNat a => DA a -> DB'

db' :: DB'
db' = DB' (da :: DA 5)
