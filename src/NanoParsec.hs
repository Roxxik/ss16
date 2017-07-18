module NanoParsec where

  import Data.Bool
  import Control.Monad
  import Control.Applicative

  import Util

  first  f (x, c) = (f x, c)
  second f (c, x) = (c, f x)

  newtype Parser i a = P { unP :: [i] -> Maybe (a, [i]) }


  item :: Parser i i
  item = P uncons

  failure :: Parser i a
  failure = P . const $ Nothing

  unit :: a -> Parser i a
  unit = P . Just .: (,)

  eoi :: Parser i ()
  eoi = P $ uncons' (Just ((), [])) (\_ _ -> Nothing)

  satisfy :: (i -> Bool) -> Parser i i
  satisfy p = do
    i <- item
    if p i then return i else empty

  oneOf :: Eq i => [i] -> Parser i i
  oneOf s = satisfy (`elem` s)

  char :: Eq i => i -> Parser i i
  char c = satisfy (c ==)

  string :: Eq i => [i] -> Parser i [i]
  string = mapM char

  force :: Parser i (Maybe a) -> Parser i a
  force p = p >>= maybe failure return

  instance Functor (Parser i) where
    fmap f (P p) = P $ \s -> first f <$> p s

  instance Applicative (Parser i) where
    pure = return
    (<*>) = ap

  instance Alternative (Parser i) where
    empty = failure
    P p <|> P q = P $ maybe <$> q <*> pure return <*> p

  instance Monad (Parser i) where
    return = unit
    P p >>= f = P $ p >=> uncurry unP . first f

  instance MonadPlus (Parser i)

  parse :: Parser i b -> [i] -> Maybe b
  parse = fmap fst .: unP . (<* eoi)
