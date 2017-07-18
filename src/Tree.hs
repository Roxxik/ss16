{-# LANGUAGE LambdaCase #-}
module Tree
  ( Tree(..)
  , Delimited(..)
  , empty
  , singleton
  , foldTree
  , unfoldTree
  , flatten
  , unflatten
  , unflattenForest
  ) where

  import NanoParsec
  import Data.Either
  import Data.Traversable
  import Control.Monad
  import Control.Applicative hiding (empty)

  data Tree a = Leaf a | Node [Tree a]
    deriving (Show, Eq)

  instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node ts) = Node (map (fmap f) ts)

  instance Applicative Tree where
    pure = return
    (<*>) = ap

  instance Monad Tree where
    return = Leaf
    Leaf x >>= f = f x
    Node ts >>= f = Node (map (>>= f) ts)

  instance Foldable Tree where
    foldMap = foldMapDefault

  instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f (Leaf x)  = Leaf <$> f x
    traverse f (Node ts) = Node <$> traverse (traverse f) ts

  empty :: Tree a
  empty = Node []

  singleton :: a -> Tree a
  singleton = Leaf

  unfoldTree :: (b -> Either a [b]) -> b -> Tree a
  unfoldTree f z = case f z of
    Left x   -> Leaf x
    Right ys -> Node (map (unfoldTree f) ys)

  foldTree :: (a -> r) -> ([r] -> r) -> Tree a -> r
  foldTree fl fn (Leaf x) = fl x
  foldTree fl fn (Node t) = fn (map (foldTree fl fn) t)

  flatten' :: Tree a -> [a]
  flatten' (Leaf x)  = [x]
  flatten' (Node ts) = concatMap flatten' ts

  between :: a -> a -> [a] -> [a]
  between a b xs = [a] ++ xs ++ [b]

  -- for unflatten to work, the delimiters should !not! be contained in the tree
  -- flatten a b t = assert (a `notElem` flatten' t && b `notElem` flatten' t) (...)
  flatten :: a -> a -> Tree a -> [a]
  flatten _ _  (Leaf x) = [x]
  flatten a b (Node ts) = between a b $ concatMap (flatten a b) ts

  data Delimited a = Start | End | Delimited a

  isStart :: Delimited a -> Bool
  isStart Start = True
  isStart _ = False

  isEnd :: Delimited a -> Bool
  isEnd End = True
  isEnd _ = False

  getDelimited :: Delimited a -> Maybe a
  getDelimited (Delimited x) = Just x
  getDelimited _ = Nothing

  tree :: Parser (Delimited a) (Tree a)
  tree = satisfy isStart *> (Node <$> forest) <* satisfy isEnd
     <|> Leaf <$> leaf

  leaf :: Parser (Delimited a) a
  leaf = force $ getDelimited <$> item

  forest :: Parser (Delimited a) [Tree a]
  forest = many tree

  unflatten :: (a -> Delimited b) -> [a] -> Maybe (Tree b)
  unflatten f = parse tree . map f

  unflattenForest :: (a -> Delimited b) -> [a] -> Maybe [Tree b]
  unflattenForest f = parse forest . map f

--  data Cxt a = Top | Cxt (Cxt a) [a] a [a]
--
--  down :: (a -> ([a],a,[a])) -> Cxt a -> Cxt a
--  down f c@(Cxt _ _ x _) = Cxt c l' x' r'
--    where (l',x',r') = f x
--
--  up :: Cxt a -> Cxt a
--  up (Cxt c _ _ _) = c
--
--  left :: Cxt a -> Maybe (Cxt a)
--  left (Cxt _ [] _ _) = Nothing
--  left (Cxt c (l:ls) x rs) = Just $ Cxt c ls l (x:rs)
--
--  right :: Cxt a -> Maybe (Cxt a)
--  right (Cxt _ _ _ []) = Nothing
--  right (Cxt c ls x (r:rs)) = Just $ Cxt c (x:ls) r rs
