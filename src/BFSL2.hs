{-# LANGUAGE TypeInType, TypeOperators, GADTs, TypeApplications, ScopedTypeVariables #-}
module BFSL2 where

import Max(Max)
import BF(Program)
import qualified BF

import Prelude hiding (id, not, or, and, until, mod, div, Maybe)
import GHC.TypeLits
import Data.Proxy(Proxy(Proxy))
import Data.Kind (Type)
import Data.Monoid((<>))

infixr 2 ***
infixr 1 >>>
infixr 1 <<<

data BFSL (a :: Nat) (s :: Nat) (b :: Nat) = BFSL { runBFSL :: Program }

data OBFSL :: Nat -> Nat -> Type where
  OBFSL :: KnownNat s => BFSL a s b -> OBFSL a b

runOBFSL :: OBFSL a b -> Program
runOBFSL (OBFSL b) = runBFSL b

runBFSL' :: BFSL a s b -> Program
runBFSL' = BF.optimize2 . runBFSL

runOBFSL' :: OBFSL a b -> Program
runOBFSL' (OBFSL b) = runBFSL' b

-- | functions to retrieve type level values  -----------------------------------

intVal :: KnownNat n => proxy n -> Int
intVal n = fromIntegral (natVal n)

rSize' :: forall i r o. (KnownNat r) => BFSL i r o -> Int
rSize' f = intVal (Proxy @r)

iSize' :: forall i r o. (KnownNat i) => BFSL i r o -> Int
iSize' f = intVal (Proxy @i)

oSize' :: forall i r o. (KnownNat o) => BFSL i r o -> Int
oSize' f = intVal (Proxy @o)


-- | primitives     --------------------------------------------------------

inc' :: BFSL 1 1 1
inc' = BFSL BF.inc

inc :: OBFSL 1 1
inc = OBFSL inc'

dec' :: BFSL 1 1 1
dec' = BFSL BF.dec

dec :: OBFSL 1 1
dec = OBFSL dec'

inp' :: BFSL 0 1 1
inp' = BFSL BF.inp

inp :: OBFSL 0 1
inp = OBFSL inp'

out' :: BFSL 1 1 1
out' = BFSL BF.out

out :: OBFSL 1 1
out = OBFSL out'

id' :: BFSL a a a
id' = BFSL mempty

id :: KnownNat a => OBFSL a a
id = OBFSL id'


-- | helper    --------------------------------------------------------

movRBy :: Int -> Program
movRBy n = mconcat $ replicate n BF.movR

movLBy :: Int -> Program
movLBy n = mconcat $ replicate n BF.movL

-- positive -> right // negative -> left
movBy :: Int -> Program
movBy n
  | n >= 0    = movRBy n
  | otherwise = movLBy (negate n)

mov :: Int -> Int -> Program
mov src dst = movBy (dst - src)

-- copy, might overlap
copy :: Int -> Int -> Int -> Program
copy src dst n = case compare src dst of
  EQ -> mempty
  GT -> copy' src dst n
  LT -> copyRev' src dst n

--copy nonoverlapping, from back to front
copyRev' :: Int -> Int -> Int -> Program
copyRev' src dst n = foldr (\d f -> copyCell (src + d) (dst + d) <> f) mempty (reverse [0 .. n - 1])

-- copy nonoverlapping
copy' :: Int -> Int -> Int -> Program
copy' src dst n = foldr (\d f -> copyCell (src + d) (dst + d) <> f) mempty [0 .. n - 1]

copyCell :: Int -> Int -> Program
copyCell src dst
  | src == dst = mempty
  | otherwise =
       movRBy src
    <> BF.while (
         mov src dst
      <> BF.inc
      <> mov dst src
      <> BF.dec
    )
    <> movLBy src

dupBF :: Int -> Int -> Int -> Program
dupBF src dst tmp =
     copyCell src tmp
  <> movRBy tmp
  <> BF.while (
       mov tmp fst
    <> BF.inc
    <> mov fst snd
    <> BF.inc
    <> mov snd tmp
    <> BF.dec
    )
  <> movLBy tmp
  where
    fst = min src dst
    snd = max src dst

-- | combinators     --------------------------------------------------------

(<<<) :: OBFSL b c -> OBFSL a b -> OBFSL a c
OBFSL f <<< OBFSL g = OBFSL (f <<<< g)

(>>>) = flip (<<<)

(<<<<) :: BFSL b s1 c -> BFSL a s2 b -> BFSL a (Max s1 s2) c
BFSL f <<<< BFSL g = BFSL (g <> f)

(****) :: (KnownNat a, KnownNat s1, KnownNat b, KnownNat c) =>
  BFSL a s1 b -> BFSL c s2 d -> BFSL (a + c) (Max (s1 + c) (b + s2)) (b + d)
f **** g = BFSL $
     copy (iSize' f) (rSize' f) (iSize' g)
  <> runBFSL f
  <> copy (rSize' f) (oSize' f) (iSize' g)
  <> movRBy (oSize' f)
  <> runBFSL g
  <> movLBy (oSize' f)

(***) :: (KnownNat a, KnownNat b, KnownNat c) =>
  OBFSL a b -> OBFSL c d -> OBFSL (a + c) (b + d)
OBFSL f *** OBFSL g = OBFSL (f **** g)

loop' :: (KnownNat a, KnownNat b) => BFSL a b a -> BFSL (a + 1) (b + 1) a
loop' f = BFSL $
     copyCell (iSize' f) (rSize' f)
  <> movRBy (rSize' f)
  <> BF.while (
       movLBy (rSize' f)
    <> runBFSL f
    <> movRBy (rSize' f)
    <> BF.dec
  )
  <> movLBy (rSize' f)

loop :: KnownNat a => OBFSL a a -> OBFSL (a + 1) a
loop (OBFSL b) = OBFSL (loop' b)

clear' :: BFSL 1 1 0
clear' = BFSL $ BF.while BF.dec

clear :: OBFSL 1 0
clear = OBFSL clear'

new' :: BFSL 0 0 b
new' = BFSL mempty

new :: OBFSL 0 b
new = OBFSL new'

dupN' :: forall n. KnownNat n => BFSL n (n + n + 1) (n + n)
dupN' = BFSL $ foldr go mempty [0 .. n - 1]
  where
    n = intVal (Proxy @n)
    go i f = dupBF i (n + i) (n + i + 1) <> f

dupN :: KnownNat n => OBFSL n (n + n)
dupN = OBFSL dupN'

swap' :: BFSL 2 3 2
swap' = BFSL $
     copy 0 1 2
  <> copyCell 2 0

swap :: OBFSL 2 2
swap = OBFSL swap'

unless' :: (KnownNat a) => BFSL a b a -> BFSL (a + 1) (Max (a + 1) b) a
unless' f = BFSL $
     movRBy (iSize' f)
  <> BF.while (
       runBFSL clear'
    <> movLBy (iSize' f)
    <> runBFSL f
    <> movRBy (iSize' f)
    )
  <> movLBy (iSize' f)

unless :: KnownNat a => OBFSL a a -> OBFSL (a + 1) a
unless (OBFSL b) = OBFSL (unless' b)

until' :: KnownNat a =>
  BFSL a c (a + 1) -> BFSL a b a -> BFSL a (Max (a + 1) (Max c b)) a
until' c b = BFSL $
     runBFSL c
  <> movRBy (iSize' b)
  <> BF.while (
       runBFSL clear'
    <> movLBy (iSize' b)
    <> runBFSL b
    <> runBFSL c
    <> movRBy (iSize' b)
    )
  <> movLBy (iSize' b)

until :: KnownNat a => OBFSL a (a + 1) -> OBFSL a a -> OBFSL a a
until (OBFSL c) (OBFSL b) = OBFSL (until' c b)

-- | convenience     --------------------------------------------------------

first :: (KnownNat d, KnownNat b, KnownNat a) => OBFSL a b -> OBFSL (a + d) (b + d)
first f = f *** id

second :: (KnownNat c, KnownNat b) => OBFSL c d -> OBFSL (b + c) (b + d)
second f = id *** f

replicateBFSL :: KnownNat a => Int -> OBFSL a a -> OBFSL a a
replicateBFSL 0 f = id
replicateBFSL n f = f >>> replicateBFSL (n - 1) f

lit :: Int -> OBFSL 0 1
lit n = new >>> replicateBFSL n inc

when :: KnownNat a => OBFSL a a -> OBFSL (a + 1) a
when f = second (first (lit 1) >>> unless dec) >>> unless f

litOp :: KnownNat a => OBFSL (a + 1) b -> Int -> OBFSL a b
litOp f n = second (lit n) >>> f

dup :: OBFSL 1 2
dup = dupN

if' :: (
  ((a + 1) + 1) ~ (a + 2),
  KnownNat a
  ) =>
  OBFSL a a -> OBFSL a a -> OBFSL (a + 1) a
if' t e = second dup >>> unless (first t) >>> when e

not :: OBFSL 1 1
not = first new >>> unless inc

isZero :: OBFSL 1 1
isZero = not

test :: (KnownNat a, KnownNat b) => OBFSL a b -> OBFSL a (a + b)
test f = dupN >>> second f

add :: OBFSL 2 1
add = loop inc

sub :: OBFSL 2 1
sub = loop dec

or :: OBFSL 2 1
or = add

and :: OBFSL 2 1
and = not *** not >>> or >>> not

diff :: OBFSL 2 2
diff = until (test and) (dec *** dec)

eq :: OBFSL 2 1
eq = sub >>> not

gt :: OBFSL 2 1
gt = diff >>> second clear

ge :: OBFSL 2 1
ge = lt >>> not

lt :: OBFSL 2 1
lt = diff >>> first clear

le :: OBFSL 2 1
le = gt >>> not

xor :: OBFSL 2 1
xor = eq >>> not

unsafeDivmod :: OBFSL 2 2
unsafeDivmod = first new >>> until (second (test ge)) (inc *** (second dup >>> first sub)) >>> second clear

divmod :: OBFSL 2 2
divmod = second (test isZero) >>> unless unsafeDivmod

unsafeMod :: OBFSL 2 2
unsafeMod = until (test ge) (second dup >>> first sub)

mod :: OBFSL 2 1
mod = second (test isZero) >>> unless unsafeMod >>> second clear

toDigits :: OBFSL 1 3
toDigits = litOp divmod 10 >>> first (litOp divmod 10) >>> first (litOp mod 10)

charLit :: Char -> OBFSL 0 1
charLit c = lit (fromEnum c)

fromDigit :: OBFSL 1 1
fromDigit = second (charLit '0') >>> add

outDigit :: OBFSL 1 1
outDigit = fromDigit >>> out

rot :: OBFSL 3 3
rot = first swap >>> second swap

outNumber :: OBFSL 1 0
outNumber =
      toDigits
  >>> first (test isZero >>> unless outDigit)
  >>> first (
          isZero *** test isZero
      >>> second rot
      >>> second and
      >>> unless outDigit
      >>> clear
      )
  >>> outDigit
  >>> clear
