{-# LANGUAGE TypeInType, GADTs, RankNTypes, TypeFamilies, ScopedTypeVariables,
  AllowAmbiguousTypes, TypeOperators, TypeApplications, ConstraintKinds,
  FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
module BFSL3 where

import Max(Max)
import qualified BF
import qualified Instr as BF
import Instr (Program)

import Prelude hiding (id, const, not, or, and, until, mod, div, Maybe, head, tail, Eq, Ord, Functor(..), Applicative(..), Monad(..), map, IO)
import GHC.TypeLits
import GHC.TypeLits.KnownNat
import GHC.Word
import Data.Kind (Type)
import Data.Monoid
import Data.Proxy
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Decide
import Data.Singletons.Prelude hiding(Max)

infixr 2 ***
infixr 1 >>>
infixr 1 <<<

--data BFSL :: Nat -> Nat -> Nat -> Type where
--  BFSL :: Program -> BFSL a s b

data BFSL :: Type -> Nat -> Type -> Type where
  BFSL :: Program -> BFSL a s b

data OBFSL :: Type -> Type -> Type where
  OBFSL :: KnownNat s => BFSL a s b -> OBFSL a b

type a :-> b = OBFSL a b

class Sized a where
  type SizeOf a :: Nat

size :: forall a. Sized' a => Int
size = intVal (Proxy @(SizeOf a))

type Sized' a = (Sized a, KnownNat (SizeOf a))

type Size a n = (Sized' a, SizeOf a ~ n)

instance Sized () where
  type SizeOf () = 0

instance (Sized a, Sized b) => Sized (a, b) where
  type SizeOf (a, b) = SizeOf a + SizeOf b

instance Sized Word8 where
  type SizeOf Word8 = 1

instance Sized Bool where
  type SizeOf Bool = 1

instance Sized Char where
  type SizeOf Char = 1

class Sized' a =>  Literal a where
  lit :: a -> () :-> a

instance Literal Word8 where
  lit n = new >>> replicateBFSL n inc

instance Literal Bool where
  lit b = new >>> if b then cast >>> inc >>> cast else id

instance Literal Char where
  lit c = lit (fromIntegral (fromEnum c) :: Word8) >>> cast

runBFSL :: BFSL a s b -> Program
runBFSL (BFSL p) = p

runOBFSL :: OBFSL a b -> Program
runOBFSL (OBFSL b) = runBFSL b

runBFSL' :: BFSL a s b -> Program
runBFSL' = BF.optimize2 . runBFSL

runOBFSL' :: OBFSL a b -> Program
runOBFSL' (OBFSL b) = runBFSL' b

intVal :: KnownNat n => proxy n -> Int
intVal n = fromIntegral (natVal n)

rSize :: forall a s b. KnownNat s => BFSL a s b -> Int
rSize f = intVal (Proxy @s)

iSize :: forall a s b. (Sized' a) => BFSL a s b -> Int
iSize f = intVal (Proxy @(SizeOf a))

oSize :: forall a s b. (Sized' b) => BFSL a s b -> Int
oSize f = intVal (Proxy @(SizeOf b))

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

clearAt :: Int -> Program
clearAt n =
     movRBy n
  <> runOBFSL (clear1 :: OBFSL Word8 ())
  <> movLBy n

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

inc' :: BFSL Word8 1 Word8
inc' = BFSL BF.inc

inc :: OBFSL Word8 Word8
inc = OBFSL inc'

dec' :: BFSL Word8 1 Word8
dec' = BFSL BF.dec

dec :: OBFSL Word8 Word8
dec = OBFSL dec'

inp' :: BFSL () 1 Char
inp' = BFSL BF.inp

inp :: OBFSL () Char
inp = OBFSL inp'

out' :: BFSL Char 1 Char
out' = BFSL BF.out

out :: OBFSL Char Char
out = OBFSL out'

id' :: BFSL a s a
id' = BFSL mempty

id :: Sized' a => OBFSL a a
id = OBFSL (id' :: BFSL a (SizeOf a) a)

(<<<) :: b :-> c -> a :-> b -> a :-> c
OBFSL f <<< OBFSL g = OBFSL (f <<<< g)

(>>>) = flip (<<<)

(<<<<) :: BFSL b s1 c -> BFSL a s2 b -> BFSL a (Max s1 s2) c
BFSL f <<<< BFSL g = BFSL (g <> f)

(****) :: (Sized' a, KnownNat s1, Sized' b, Sized' c) =>
  BFSL a s1 b -> BFSL c s2 d -> BFSL (a,  c) (Max (s1 + SizeOf c) (SizeOf b + s2)) (b, d)
f **** g = BFSL $
     copy (iSize f) (rSize f) (iSize g)
  <> runBFSL f
  <> copy (rSize f) (oSize f) (iSize g)
  <> movRBy (oSize f)
  <> runBFSL g
  <> movLBy (oSize f)

(***) :: (Sized' a, Sized' b, Sized' c) => a :-> b -> c :-> d -> (a, c) :-> (b, d)
OBFSL f *** OBFSL g = OBFSL (f **** g)

loop' :: (Sized' a, KnownNat s) => BFSL a s a -> BFSL (a, Word8) (s + SizeOf Word8) a
loop' f = BFSL $
     copyCell (iSize f) (rSize f)
  <> movRBy (rSize f)
  <> BF.while (
       movLBy (rSize f)
    <> runBFSL f
    <> movRBy (rSize f)
    <> BF.dec
  )
  <> movLBy (rSize f)

loop :: Sized' a => OBFSL a a -> OBFSL (a, Word8) a
loop (OBFSL b) = OBFSL (loop' b)

clear1' :: Size a 1 => BFSL a (SizeOf a) ()
clear1' = BFSL $ BF.while BF.dec

clear1 :: Size a 1 => OBFSL a ()
clear1 = OBFSL clear1'

new' :: BFSL () 0 b
new' = BFSL mempty

new :: OBFSL () b
new = OBFSL new'

unsafeCast' :: (Sized' a, Sized' b) => BFSL a (Max (SizeOf a) (SizeOf b)) b
unsafeCast' = BFSL mempty

-- only ever call this if you are sure that SizeOf a ~ SizeOf b,
-- but you can't prove it due to non injectivity of seom type family
unsafeCast :: (Sized' a, Sized' b) => OBFSL a b
unsafeCast = OBFSL unsafeCast'

cast :: (Sized' a, Sized' b, SizeOf a ~ SizeOf b) => OBFSL a b
cast = unsafeCast

dup' :: forall n. Sized' n => BFSL n (SizeOf n + SizeOf n + 1) (n, n)
dup' = BFSL $ foldr go mempty [0 .. n - 1]
  where
    n = intVal (Proxy @(SizeOf n))
    go i f = dupBF i (n + i) (n + i + 1) <> f

dup :: Sized' n => OBFSL n (n, n)
dup = OBFSL dup'

clear' :: forall a. Sized' a => BFSL a (SizeOf a) ()
clear' = BFSL $ foldr (\i f -> clearAt i <> f) mempty [0..intVal (Proxy @(SizeOf a)) - 1]

clear :: Sized' a => OBFSL a ()
clear = OBFSL clear'

swap' :: forall a b. (Sized' a, Sized' b) =>
  BFSL (a, b) (SizeOf a + SizeOf b + SizeOf a) (b, a)
swap' = BFSL $
     copy 0 (sa + sb) sa -- copy sa
  <> copy sa 0 sb -- copy sb
  <> copy (sa + sb) sb sa -- copy sa back
    where
      sa = size @a
      sb = size @b

swap :: (Sized' a, Sized' b) => OBFSL (a, b) (b, a)
swap = OBFSL swap'

-- this is broken
if'' :: (Sized' a, KnownNat s1, KnownNat s2) =>
  BFSL a s1 b -> BFSL a s2 b -> BFSL (a, Bool) (2 + Max s1 s2) b
if'' t f = BFSL $
     copyCell (iSize t) cond -- this assumes SizeOf Bool == 1
  <> movRBy cond
  <> BF.movR
  <> BF.inc
  <> BF.movL
  <> BF.while (
       BF.dec
    <> movLBy cond
    <> runBFSL t
    <> movRBy cond
    <> BF.movR
    <> BF.dec
    <> BF.movL
    )
  <> copyCell 1 0 -- this assumes SizeOf Bool == 1
  <> BF.while (
       BF.dec
    <> movLBy cond
    <> runBFSL f
    <> movRBy cond
    )
  <> movLBy (rSize f)
    where
      cond = max (rSize t) (rSize f)

--if' :: Sized' a => a :-> b -> a :-> b -> (a, Bool) :-> b
--if' (OBFSL t) (OBFSL f) = OBFSL (if'' t f)

while' :: Sized' a =>
  BFSL a sc (a, Bool) -> BFSL a sb a -> BFSL a (Max (SizeOf (a, Bool)) (Max sc sb)) a
while' c b = BFSL $
     runBFSL c
  <> movRBy (iSize b)
  <> BF.while (
       BF.dec
    <> movLBy (iSize b)
    <> runBFSL b
    <> runBFSL c
    <> movRBy (iSize b)
    )
  <> movLBy (iSize b)

while :: Sized' a => a :-> (a, Bool) -> a :-> a -> a :-> a
while (OBFSL c) (OBFSL b) = OBFSL (while' c b)

const :: (Sized' a, Sized' b) => () :-> b -> a :-> b
const f = clear >>> f

first :: (Sized' a, Sized' b, Sized' d) => a :-> b -> (a, d) :-> (b, d)
first f = f *** id

second :: (Sized' b, Sized' c) => c :-> d -> (b, c) :-> (b, d)
second f = id *** f

elimL :: Sized' a => ((), a) :-> a
elimL = cast

elimR :: Sized' a => (a, ()) :-> a
elimR = cast

initL :: Sized' a => a :-> ((), a)
initL = cast

initR :: Sized' a => a :-> (a, ())
initR = cast

newL :: (Sized' a, Sized' b) => () :-> b -> a :-> (b, a)
newL f = initL >>> first f

newR :: (Sized' a) => () :-> b -> a :-> (a, b)
newR f = initR >>> second f

delL :: (Sized' a, Sized' b) => (a, b) :-> b
delL = first clear >>> elimL

delR :: (Sized' a, Sized' b) => (a, b) :-> a
delR = second clear >>> elimR

assocL :: (
  Sized' a, Sized' b, Sized' c
  ) =>
  (a, (b, c)) :-> ((a, b), c)
assocL = unsafeCast

assocR :: (
  Sized' a, Sized' b, Sized' c
  ) =>
  ((a, b), c) :-> (a, (b, c))
assocR = unsafeCast

replicateBFSL :: (Sized' a, Integral n) => n -> a :-> a -> a :-> a
replicateBFSL 0 f = id
replicateBFSL n f = f >>> replicateBFSL (n - 1) f

not :: Bool :-> Bool
not = newL (lit 0) >>> unless inc >>> cast

unless :: forall a. Sized' a => a :-> a -> (a, Bool) :->  a
unless f = second (newL (lit True) >>> when (cast >>> dec >>> cast)) >>> f'
  where
    f' :: (a, Bool) :-> a
    f' = when f


when' :: Sized' a => BFSL a b a -> BFSL (a, Bool) (b + 1) a
when' f = BFSL $
     movRBy (iSize f)
  <> BF.while (
       runBFSL (clear' :: BFSL Word8 1 ())
    <> movLBy (iSize f)
    <> runBFSL f
    <> movRBy (iSize f)
    )
  <> movLBy (iSize f)

when :: Sized' a => a :-> a -> (a, Bool) :->  a
when (OBFSL b) = OBFSL (when' b)

test :: (Sized' a) => (a :-> b) -> (a :-> (a, b))
test f = dup >>> second f

add :: (Word8, Word8) :-> Word8
add = loop inc

sub :: (Word8, Word8) :-> Word8
sub = loop dec

or :: (Bool, Bool) :-> Bool
or = when (const (lit True))

and :: (Bool, Bool) :-> Bool
and = unless (const (lit False))

bool :: Word8 :-> Bool
-- this is inefficient
bool = newL (lit False) >>> loop (const (lit True))

isZero :: Word8 :-> Bool
isZero = bool >>> not

diff :: (Word8, Word8) :-> (Word8, Word8)
diff = while (test (bool *** bool >>> and)) (dec *** dec)

class Eq a where
  eq :: (a,a) :-> Bool

instance Eq Word8 where
  eq = sub >>> isZero

instance Eq Char where
  eq = cast *** cast >>> eq @Word8 >>> cast

class Ord a where
  gt :: (a, a) :-> Bool
  ge :: (a, a) :-> Bool
  lt :: (a, a) :-> Bool
  le :: (a, a) :-> Bool

instance Ord Word8 where
  gt = diff >>> delR >>> bool
  ge = lt >>> not
  lt = diff >>> delL >>> bool
  le = gt >>> not

instance Ord Char where
  gt = cast *** cast >>> gt @Word8 >>> cast
  ge = cast *** cast >>> ge @Word8 >>> cast
  lt = cast *** cast >>> lt @Word8 >>> cast
  le = cast *** cast >>> le @Word8 >>> cast

xor :: (Word8, Word8) :-> Bool
xor = eq >>> not

unsafeMod :: (Word8, Word8) :-> (Word8, Word8)
unsafeMod = while (test ge) (second dup >>> assocL >>> first sub)

mod :: (Word8, Word8) :-> Word8
mod = second (test bool) >>> assocL >>> when unsafeMod >>> delR

unsafeDivmod :: (Word8, Word8) :-> (Word8, Word8)
unsafeDivmod =
      newL (lit 0)
  >>> while (second (test ge) >>> assocL) (inc *** (second dup >>> assocL >>> first sub))
  >>> assocL
  >>> delR

divmod :: (Word8, Word8) :-> (Word8, Word8)
divmod = second (test bool) >>> assocL >>> when unsafeDivmod

litOp :: (Sized' a, Literal b) => ((a, b) :-> c) -> b -> (a :-> c)
litOp f n = newR (lit n) >>> f

toDigits :: Word8 :-> (Word8, (Word8, Word8))
toDigits =
      litOp divmod 10
  >>> first (litOp divmod 10)
  >>> assocR
  >>> first (litOp mod 10)

rot :: (
  Sized' a, Sized' b, Sized' c,
  (SizeOf a + (SizeOf b + SizeOf c)) ~ ((SizeOf a + SizeOf b) + SizeOf c),
  (SizeOf b + (SizeOf a + SizeOf c)) ~ ((SizeOf b + SizeOf a) + SizeOf c),
  (SizeOf b + (SizeOf c + SizeOf a)) ~ ((SizeOf b + SizeOf c) + SizeOf a)
  ) =>
  (a, (b, c)) :-> ((b, c), a)
rot = swap

fromDigit :: Word8 :-> Char
fromDigit = litOp (second cast >>> add) '0' >>> cast

toDigit :: Char :-> Word8
toDigit = litOp (cast *** cast >>> sub) '0' >>> cast

outDigit :: Word8 :-> Char
outDigit = fromDigit >>> out

outNumber :: Word8 :-> ()
outNumber =
      toDigits
  >>> first (test bool >>> when (dup >>> second outDigit >>> delR))
  >>> assocL
  >>> first (
          bool *** test bool
      >>> rot
      >>> assocR
      >>> second or
      >>> when (dup >>> second outDigit >>> delR)
      >>> clear
      )
  >>> elimL
  >>> outDigit
  >>> clear

class Functor f where
  map :: (Sized' a, Sized' b) => (a :-> b) -> (f a :-> f b)

class Functor f => Applicative f where
  pure :: Sized' a => a :-> f a
  seq :: (Sized' a, Sized' b) => (f a, f b) :-> f (a, b)

class Applicative m => Monad m where
  return :: Sized' a => a :-> m a
  return = pure

  bind :: (Sized' a, Sized' b) => (a :-> m b) -> (m a :-> m b)

  kleisli :: (Sized' a, Sized' b, Sized' c) => (a :-> m b) -> (b :-> m c) -> (a :-> m c)

  join :: Sized' a => m (m a) :-> m a

data Maybe :: Type -> Type

fromMaybe :: Sized' a => Maybe a :-> (a, Bool)
fromMaybe = cast

toMaybe :: Sized' a => (a, Bool) :-> Maybe a
toMaybe = cast

instance Sized a => Sized (Maybe a) where
  type SizeOf (Maybe a) = SizeOf a + 1

just :: Sized' a => a :-> Maybe a
just = newR (lit True) >>> toMaybe

nothing :: forall a. Sized' a => () :-> Maybe a
nothing = new @a >>> newR (lit False) >>> toMaybe

instance Functor Maybe where
  -- map :: (Sized' a, Sized' b) => a :-> b -> Maybe a :-> Maybe b
  map f = undefined -- fromMaybe >>> if' (f >>> just) (const nothing)

instance Applicative Maybe where
  pure = just
  --seq :: (Maybe a, Maybe b) :-> Maybe (a, b)
  seq = fromMaybe *** fromMaybe
    >>> assocL
    >>> first (
          assocR
      >>> second swap
      >>> assocL
    )
    >>> assocR
    >>> second and
    >>> toMaybe

instance Monad Maybe where
  return = just
  join = fromMaybe >>> first fromMaybe >>> assocR >>> second and >>> toMaybe
  bind f = map f >>> join
  kleisli f g = f >>> map g >>> join

data Vec :: Nat -> Type -> Type

instance Sized a => Sized (Vec n a) where
  type SizeOf (Vec n a) = SizeOf a :* n

cons :: (KnownNat n, Sized' a) =>
  (Vec n a, a) :-> Vec (n + 1) a
cons = unsafeCast

uncons :: (KnownNat n, Sized' a) =>
  Vec n a :-> (Vec (n - 1) a, a)
uncons = unsafeCast

append :: (KnownNat n, KnownNat m, Sized' a) =>
  (Vec n a, Vec m a) :-> Vec (n + m) a
append = unsafeCast

split :: (KnownNat n, KnownNat m, Sized' a) =>
  Vec (n + m) a :-> (Vec n a, Vec m a)
split = unsafeCast

head :: (
  SizeOf a <= n :* SizeOf a,
  Sized' a, KnownNat n
  ) =>
  Vec n a :-> a
head = uncons >>> delL

tail :: (
  SizeOf a <= n :* SizeOf a,
  Sized' a, KnownNat n
  ) =>
  Vec n a :-> Vec (n - 1) a
tail = uncons >>> delR

singleton :: Sized' a => a :-> Vec 1 a
singleton = newL new >>> cons

data IO a

instance Sized a => Sized (IO a) where
  type SizeOf (IO a) = SizeOf a

toIO :: Sized' a => a :-> IO a
toIO = cast

fromIO :: Sized' a => IO a :-> a
fromIO = cast

instance Functor IO where
  map f = fromIO >>> f >>> toIO

instance Applicative IO where
  pure = toIO
  seq = fromIO *** fromIO >>> toIO

instance Monad IO where
  return = toIO
  join = fromIO
  bind f = map f >>> join
  kleisli f g = f >>> map g >>> join

getChar :: () :-> IO Char
getChar = inp >>> return

putChar :: Char :-> IO ()
putChar = out >>> clear >>> return

--type Parser a = Char :-> IO (Maybe a)
{-

item :: Parser 1 Char
item = just head

fail :: Sized' a => Parser 0 a
fail = const nothing

unit :: (Sized' a) => (Char :-> a) -> Parser 1 a
unit f = just (head >>> f)

satisfy :: (Char :-> Bool) -> Parser 1 Char
satisfy p = head >>> test p

mapParser :: (Sized' a, Sized' b) => OBFSL a b -> Parser n a -> Parser n b
mapParser f p = p >>> mapMaybe f

seqParser :: forall a b n m. (
  Sized' a, Sized' b,
  ((SizeOf a + 1) + (SizeOf b + 1)) ~ (((SizeOf a + 1) + SizeOf b) + 1),
  (SizeOf a + (1 + SizeOf b)) ~ ((SizeOf a + 1) + SizeOf b),
  (SizeOf a + (SizeOf b + 1)) ~ ((SizeOf a + SizeOf b) + 1),
  ((SizeOf a + SizeOf b) + 2) ~ (((SizeOf a + SizeOf b) + 1) + 1),
  KnownNat n, KnownNat m
  ) =>
  Parser n a -> Parser m b -> Parser (n + m) (a, b)
seqParser p q = split >>> p *** q >>> appendMaybe

constParser :: (
  Sized' a, Sized' b,
  (SizeOf a + 2) ~ ((SizeOf a + 1) + 1),
  KnownNat n, KnownNat m
  ) => Parser n a -> Parser m b -> Parser (n + m) a
constParser p q = mapParser elimR (seqParser p (mapParser clear q))

runParser :: (
  Sized' a,
  (SizeOf a + 2) ~ ((SizeOf a + 1) + 1),
  KnownNat n
  ) => Parser n a -> (Vec (n + 1) Char :-> Maybe a)
runParser p = constParser p (satisfy (litOp eq '\x00'))

isDigit :: Char :-> Bool
isDigit =
      test (litOp ge '0')
  >>> first (litOp le '9')
  >>> and

digit' :: Parser 1 Word8
digit' = mapParser toDigit (satisfy isDigit)

digit3' :: Parser 3 (Word8, (Word8, Word8))
digit3' = seqParser digit' (seqParser digit' digit')

digit :: Parser 1 (Vec 1 Word8)
digit = mapParser singleton digit'

digit3 :: Parser 3 (Vec 3 Word8)
digit3 = mapParser append (seqParser digit (mapParser append (seqParser digit digit)))


option :: Parser n a -> Parser m a -> Parser _ a
option p q =




--digits' :: (
--  (1 + n) ~ (n + 1),
--  (2 + n) ~ (n + 2),
--  KnownNat n
--  ) => forall n. SNat n -> Parser (Vec n Word8)
--digits' _ =

--digits' (SS m) = mapParser append (seqParser digit (digits' m))
-}
