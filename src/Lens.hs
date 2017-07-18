{-# LANGUAGE RankNTypes #-}
module Lens where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type ASetter' s a = ASetter s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
type Getting r s a = (a -> Const r a) -> s -> Const r s
type LensLike f s t a b = (a -> f b) -> s -> f t
type LensLike' f s a = LensLike f s s a a

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)

view :: Getting a s a -> s -> a
view = flip (^.)

views :: MonadReader s m => LensLike' (Const r) s a -> (a -> r) -> m r
views l f = asks (getConst . l (Const . f))

use :: MonadState s m => Getting a s a -> m a
use l = gets (view l)

uses :: MonadState s m => Getting r s a -> (a -> r) -> m r
uses l f = gets (views l f)

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)

(<~) :: MonadState s m => ASetter s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)

(+=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l += b = modify (l +~ b)

(-~) :: Num a => ASetter s t a a -> a -> s -> t
l -~ n = over l (\x -> x - n)

(-=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l -= b = modify (l -~ b)
