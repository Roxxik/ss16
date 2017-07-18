{-# LANGUAGE TemplateHaskell, TypeApplications, KindSignatures, DataKinds,
  TypeOperators, TypeFamilies, UndecidableInstances, MultiParamTypeClasses,
  ExistentialQuantification, FlexibleInstances, ScopedTypeVariables #-}

module Max (Max) where

import GHC.TypeLits
import GHC.TypeLits.KnownNat
import Data.Proxy(Proxy(Proxy))
import Data.Type.Bool (If)
import Data.Singletons.TH (genDefunSymbols)

type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max 0 b = b
  Max a b = If (a <=? b) b a

$(genDefunSymbols [''Max]) -- creates the 'MaxSym0' symbol

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''Max) a b where
  type KnownNatF2 $(nameToSymbol ''Max) = MaxSym0
  natSing2 = let x = natVal (Proxy @a)
                 y = natVal (Proxy @b)
                 z = max x y
             in  SNatKn z
  {-# INLINE natSing2 #-}
