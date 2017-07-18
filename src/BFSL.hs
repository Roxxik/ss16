{-# LANGUAGE TypeInType, TypeOperators, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module BFSL where

import BF(Program)
import qualified BF

import Prelude hiding (id, not, or, and, until, mod, div, Maybe)
import GHC.TypeLits
import GHC.TypeLits.KnownNat
import Data.Singletons.Prelude.Ord(Max)
import Data.Proxy(Proxy(Proxy))

infixr 2 ***
infixr 1 >>>

(<>) :: Monoid a => a -> a -> a
a <> b = mappend a b

intVal :: KnownNat n => proxy n -> Int
intVal n = fromIntegral (natVal n)

data BFSL (i :: Nat) (r :: Nat) (o :: Nat) = BFSL { runBFSL :: Program }

runBFSL' = BF.optimize2 . runBFSL

inc :: BFSL 1 1 1
inc = BFSL BF.inc

dec :: BFSL 1 1 1
dec = BFSL BF.dec

inp :: BFSL 0 1 1
inp = BFSL BF.inp

out :: BFSL 1 1 1
out = BFSL BF.out

id :: BFSL a b a
id = BFSL mempty

id' :: BFSL a a a
id' = id

rProxy :: BFSL i r o -> Proxy r
rProxy _ = Proxy

rSize :: (KnownNat r) => BFSL i r o -> Int
rSize f = intVal $ rProxy f

iProxy :: BFSL i r o -> Proxy i
iProxy _ = Proxy

iSize :: (KnownNat i) => BFSL i r o -> Int
iSize f = intVal $ iProxy f

oProxy :: BFSL i r o -> Proxy o
oProxy _ = Proxy

oSize :: (KnownNat o) => BFSL i r o -> Int
oSize f = intVal $ oProxy f

(<<<) :: BFSL c d e -> BFSL a b c -> BFSL a (Max b d) e
BFSL f <<< BFSL g = BFSL  (g <> f)

(>>>) :: BFSL a b c -> BFSL c d e -> BFSL a (Max b d) e
(>>>) = flip (<<<)

movRBy :: Int -> Program
movRBy n = mconcat $ replicate n BF.movR

movLBy :: Int -> Program
movLBy n = mconcat $ replicate n BF.movL

-- positive -> right // negative -> left
movBy :: Int -> Program
movBy n
  | n >= 0    = movRBy n
  | otherwise = movLBy (negate n)

(***) :: (KnownNat i1, KnownNat r1, KnownNat o1, KnownNat i2) =>
  BFSL i1 r1 o1 -> BFSL i2 r2 o2 -> BFSL (i1 + i2) (Max (r1 + i2) (o1 + r2)) (o1 + o2)
f *** g = BFSL $
     move (iSize f) (rSize f) (iSize g)
  <> runBFSL f
  <> move (rSize f) (oSize f) (iSize g)
  <> movRBy (oSize f)
  <> runBFSL g
  <> movLBy (oSize f)

-- move, might overlap
move :: Int -> Int -> Int -> Program
move src dst n = case compare src dst of
  EQ -> mempty
  GT -> copy src dst n
  LT -> copyRev src dst n

--copy nonoverlapping, from back to front
copyRev :: Int -> Int -> Int -> Program
copyRev src dst n = foldr (\d f -> copyCell (src + d) (dst + d) <> f) mempty (reverse [0 .. n - 1])

-- copy nonoverlapping
copy :: Int -> Int -> Int -> Program
copy src dst n = foldr (\d f -> copyCell (src + d) (dst + d) <> f) mempty [0 .. n - 1]

copyCell :: Int -> Int -> Program
copyCell src dst
  | src == dst = mempty
  | otherwise =
       movRBy src
    <> BF.while (
         movBy (dst - src)
      <> BF.inc
      <> movBy (src - dst)
      <> BF.dec
    )
    <> movLBy src

loop :: (KnownNat r, KnownNat i) => BFSL i r o -> BFSL (i + 1) (r + 1) o
loop f = BFSL $
     copyCell (iSize f) (rSize f)
  <> movRBy (rSize f)
  <> BF.while (
       movLBy (rSize f)
    <> runBFSL f
    <> movRBy (rSize f)
    <> BF.dec
  )
  <> movLBy (rSize f)

clear :: BFSL 1 1 0
clear = BFSL $ BF.while BF.dec

new :: BFSL 0 0 a
new = BFSL mempty

dup :: BFSL 1 3 2
dup = dupN

dupBF :: Int -> Int -> Int -> Program
dupBF src dst tmp =
     copyCell src tmp
  <> movRBy tmp
  <> BF.while (
       movBy (max src dst - tmp)
    <> BF.inc
    <> movBy (min src dst - max src dst)
    <> BF.inc
    <> movBy (tmp - min src dst)
    <> BF.dec
    )
  <> movLBy tmp


dupN :: KnownNat n => BFSL n (n + n + 1) (n + n)
dupN = dupN' Proxy

dupN' :: KnownNat n => Proxy n -> BFSL n (n + n + 1) (n + n)
dupN' n = dupN'' (intVal n)

dupN'' :: Int -> BFSL n (n + n + 1) (n + n)
dupN'' n = BFSL $ foldr (go n) mempty [0 .. n - 1]
  where go n i f = dupBF i (n + i) (n + i + 1) <> f

clearAt :: Int -> Program
clearAt n =
     movRBy n
  <> runBFSL clear
  <> movLBy n

clearN :: KnownNat n => BFSL n n 0
clearN = clearN' Proxy

clearN' :: KnownNat n => Proxy n -> BFSL n n 0
clearN' n = clearN'' (intVal n)

clearN'' :: Int -> BFSL n n 0
clearN'' n = BFSL $ foldr (\i f -> clearAt i <> f) mempty [0..n - 1]

swap :: BFSL 2 3 2
swap = BFSL $
     move 0 1 2
  <> copyCell 2 0

unlessZ :: (KnownNat a) => BFSL a b a -> BFSL (a + 1) (Max (a + 1) b) a
unlessZ f = BFSL $
     movRBy (iSize f)
  <> BF.while (
       runBFSL clear
    <> movLBy (iSize f)
    <> runBFSL f
    <> movRBy (iSize f)
    )
  <> movLBy (iSize f)

whenZ :: (Max (a + 2) (Max (a + 1) b) ~ Max (a + 2) b, Max (a + 1) (a + 2) ~ (a + 2), KnownNat a) =>
  BFSL a b a -> BFSL (a + 1) (Max (a + 2) b) a
whenZ f = second (first (lit 1) >>> unlessZ dec) >>> unlessZ f

unless :: (Max (a + 2) (Max (a + 1) b) ~ Max (a + 2) b, Max (a + 1) (a + 2) ~ (a + 2), KnownNat a) =>
  Int -> BFSL a b a -> BFSL (a + 1) (Max (a + 2) b) a
unless n f = second (litOp sub n) >>> unlessZ f

when :: (Max (a + 1) (a + 2) ~ (a + 2), Max (a + 2) (Max (a + 1) b) ~ Max (a + 2) b, Max (a + 2) (Max (a + 2) b) ~ Max (a + 2) b, KnownNat a) =>
  Int -> BFSL a b a -> BFSL (a + 1) (Max (a + 2) b) a
when n f = second (litOp sub n) >>> whenZ f

if' :: (
  Max (i + 1) (i + 3) ~ (i + 3),
  Max (i + 1) (i + 2) ~ (i + 2),
  ((i + 1) + 1) ~ (i + 2),
  Max (i + 2) (Max (r1 + 1) (i + 1)) ~ Max (i + 2) (r1 + 1),
  Max (i + 2) (Max (i + 1) r2) ~ Max (i + 2) r2,
  Max (Max (i + 2) (r1 + 1)) (Max (i + 2) r2) ~ Max (Max (i + 2) (r1 + 1)) r2,
  Max (i + 3) (Max (Max (i + 2) (r1 + 1)) r2) ~ Max (i + 3) (Max (r1 + 1) r2),
  KnownNat (i + 1),
  KnownNat i,
  KnownNat r1) =>
  BFSL i r1 i -> BFSL i r2 i -> BFSL (i + 1) (Max (i + 3) (Max (r1 + 1) r2)) i
if' t e = second dup >>> unlessZ (t *** id') >>> whenZ e

until :: KnownNat a =>
  BFSL a c (a + 1) -> BFSL a b a -> BFSL a (Max (a + 1) (Max c b)) a
until c b = BFSL $
     runBFSL c
  <> movRBy (iSize b)
  <> BF.while (
       runBFSL clear
    <> movLBy (iSize b)
    <> runBFSL b
    <> runBFSL c
    <> movRBy (iSize b)
    )
  <> movLBy (iSize b)


first :: (Max (r + a) (o + a) ~ (Max r o + a), KnownNat a, KnownNat r, KnownNat i, KnownNat o) =>
  BFSL i r o -> BFSL (i + a) (Max r o + a) (o + a)
first f = f *** id'

second :: (Max (a + i) (a + r) ~ (a + Max i r), KnownNat i, KnownNat a) =>
  BFSL i r o -> BFSL (a + i) (a + Max i r) (a + o)
second f = id' *** f

add :: BFSL 2 2 1
add = loop inc

sub :: BFSL 2 2 1
sub = loop dec

replicateBFSL :: Int -> BFSL a b a -> BFSL a b a
replicateBFSL 0 f = id
replicateBFSL n f = f >>> replicateBFSL (n - 1) f

lit :: Int -> BFSL 0 1 1
lit n = new >>> replicateBFSL n inc

litOp :: (((i - 1) + 1) ~ i, Max (i - 1) i ~ i, KnownNat (i - 1))
  => BFSL i r o -> Int -> BFSL (i - 1) (Max i r)  o
litOp f n = second (lit n) >>> f

charLit :: Char -> BFSL 0 1 1
charLit c = lit (fromEnum c)

putCharBF :: Char -> BFSL 0 1 1
putCharBF c = charLit c >>> out

putStrBF :: String -> BFSL 0 1 0
putStrBF = foldr (\c f -> putCharBF c >>> clear >>> f) id

toDigit :: BFSL 1 2 1
toDigit = second (charLit '0') >>> sub

fromDigit :: BFSL 1 2 1
fromDigit = second (charLit '0') >>> add

inpDigit :: BFSL 0 2 1
inpDigit = inp >>> toDigit

outDigit :: BFSL 1 2 1
outDigit = fromDigit >>> out

fib :: BFSL 2 4 2
fib = second dup >>> first swap >>> second add

not :: BFSL 1 4 1
not = first new >>> if' id' inc

isZero :: BFSL 1 4 1
isZero = not

eq :: BFSL 2 4 1
eq = sub >>> not

gt :: BFSL 2 7 1
gt = diff >>> second clear

ge :: BFSL 2 7 1
ge = lt >>> not

lt :: BFSL 2 7 1
lt = diff >>> first clear

le :: BFSL 2 7 1
le = gt >>> not

xor :: BFSL 2 4 1
xor = eq >>> not

diff :: BFSL 2 7 2
diff = until (test and) (dec *** dec)

and :: BFSL 2 5 1
and = not *** not >>> or >>> not

or :: BFSL 2 2 1
or = add

test :: (
  Max (i + i) (i + r) ~ (i + Max i r),
  KnownNat i) =>
  BFSL i r o -> BFSL i (Max (i + i + 1) (i + Max i r)) (i + o)
test f = dupN >>> second f

mod :: BFSL 2 9 1
mod = second dup >>> second isZero >>> if' id' unsafeMod >>> second clear

unsafeMod :: BFSL 2 9 2
unsafeMod = until (test ge) (second dup >>> first sub)

mul :: BFSL 2 5 1
mul = first new >>> loop (second dup >>> first add) >>> second clear

div :: BFSL 2 10 1
div = divmod >>> second clear

divmod :: BFSL 2 10 2
divmod = second dup >>> second isZero >>> if' id' unsafeDivmod

unsafeDivmod :: BFSL 2 10 2
unsafeDivmod = first new >>> until (second (test ge)) (inc *** (second dup >>> first sub)) >>> second clear

toDigits :: BFSL 1 11 3
toDigits = litOp divmod 10 >>> first (litOp divmod 10) >>> first (litOp mod 10)

fromDigits :: BFSL 3 7 1
fromDigits = first (litOp mul 10) >>> first add >>> first (litOp mul 10) >>> first add

rot :: BFSL 3 4 3
rot = first swap >>> second swap

outNumber :: BFSL 1 11 0
outNumber =
      toDigits
  >>> first (test isZero >>> if' id' outDigit)
  >>> first (
          isZero *** test isZero
      >>> second rot
      >>> second and
      >>> if' id' outDigit
      >>> clear
      )
  >>> outDigit
  >>> clear

isDigit :: BFSL 1 8 1
isDigit = dup *** charLit '0' >>> second ge >>> swap >>> second (charLit '9') >>> second le >>> and

--inpNumber :: BFSL 0 _ 2
--inpNumber =
--      inp
--  >>> _


type Maybe a = a + 1

just :: (KnownNat a, KnownNat b, KnownNat c) =>
  BFSL a b c -> BFSL a (Max b (Maybe c)) (Maybe c)
just v = v *** lit 1

nothing :: BFSL 0 2 (Maybe 1)
nothing = lit 0 *** lit 0

type Parser a b c = BFSL a b (Maybe c)

item :: BFSL 0 2 (Maybe 1)
item = just inp

fail :: BFSL 0 2 (Maybe 1)
fail = nothing

unit :: (KnownNat a, KnownNat b, KnownNat c) =>
  BFSL a b c -> BFSL a (Max b (Maybe c)) (Maybe c)
unit = just

satisfy :: (Max 2 (1 + b) ~ (1 + Max 1 b)) =>
  BFSL 1 b 1 -> BFSL 0 (Max 1 (Max 3 (1 + Max 1 b))) (Maybe 1)
satisfy p = inp >>> test p

fmapMaybe :: (
  Max (b + 1) (c + 1) ~ (Max b c + 1),
  KnownNat a,
  KnownNat b,
  KnownNat c
  ) =>
  BFSL a b c -> BFSL (Maybe a) (Max b c + 1) (Maybe c)
fmapMaybe = first

bindMaybe :: (
  Max (b + 1) ((c + 1) + 1) ~ (Max b (c + 1) + 1),
  Max (Max b (c + 1) + 1) (c + 5) ~ Max (b + 1) (c + 5),
  (c + 2) ~ ((c + 1) + 1),
  Max (c + 2) (c + 5) ~ (c + 5),
  KnownNat a,
  KnownNat b,
  KnownNat c,
  KnownNat (Maybe c)
  ) => BFSL a b (Maybe c) -> BFSL (Maybe a) (Max (b + 1) (c + 5)) (Maybe c)
bindMaybe f = first f >>> second and

--times3 :: BFSL 0 b (Maybe c) -> BFSL 0 _ (Maybe (c + c + c))
--times3 p =

--resize :: (KnownNat a, KnownNat b) => BFSL a (Max a b) b
--resize = resize' Proxy Proxy
--
--resize' :: (KnownNat a, KnownNat b) => Proxy a -> Proxy b -> BFSL a (Max a b) b
--resize' p q = resize'' (intVal p) (intVal q)
--
--resize'' :: Int -> Int -> BFSL a (Max a b) b
--resize'' i o = case compare i o of
--  EQ -> BFSL (return ())
--  LT -> extend (id :: BFSL 0 0 0)
--
--extend :: (
--  a <= d,
--  b <= e,
--  c <= f
--  ) =>
--  BFSL a b c -> BFSL d e f
--extend (BFSL f) = BFSL f
--
--retract :: (
--  d <= c
--  ) =>
--  BFSL a b c -> BFSL a b d
--retract f = f >>> second clearN
