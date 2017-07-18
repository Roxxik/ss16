{-# LANGUAGE
    LambdaCase
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , UndecidableInstances
  #-}

module Next where

import Data.Word

import Control.Monad.State
import Control.Monad.RWS

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

class Monad m => MonadNext e m | m -> e where
  next :: m (Maybe e)

newtype Next a = N { unN :: State ByteString a }
  deriving (Functor, Applicative, Monad)

runNext :: Next a -> ByteString -> (a, ByteString)
runNext (N n) = runState n

instance MonadNext Word8 Next where
  next = N $ do
    bs <- get
    case B.uncons bs of
      Nothing -> return Nothing
      Just (x,xs) -> put xs >> return (Just x)

instance (Monoid w, MonadNext n m) => MonadNext n (RWST r w s m) where
  next = lift next
