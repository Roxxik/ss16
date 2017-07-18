module Util where


  infixr 9 .:
  (.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
  (.:) = (.) . (.)

  uncons' :: z -> (a -> [a] -> z) -> [a] -> z
  uncons' fn fl []     = fn
  uncons' fn fl (x:xs) = fl x xs


  uncons :: [a] -> Maybe (a, [a])
  uncons = uncons' Nothing (Just .: (,))

  (<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
  (<$$>) = fmap . fmap
