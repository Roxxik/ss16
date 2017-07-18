{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
  UndecidableInstances #-}
module Opt where

import Tree
import Lens
import Instr
import Util

import Control.Monad.State
import Control.Monad.Writer
import ListT (ListT(..))
import Data.Functor.Identity


import GHC.Word

data Set where
  InpSet :: Set
  ZroSet :: Set
  IncSet :: Set -> Set
  DecSet :: Set -> Set
  deriving Eq

lopSet :: Set -> [Set]
lopSet = undefined

data Tape = Tape Set [Set] Set [Set]

moveL :: Tape -> Tape
moveL = putHead . getHead

getHead :: Tape -> (Set, Tape)
getHead t@(Tape d ls c rs) = case uncons ls of
  Nothing     -> (d, t)
  Just (x,xs) -> (x, Tape d xs c rs)

putHead :: (Set, Tape) -> Tape
putHead (h, Tape d ls c rs)
  | c == d && null rs = Tape d ls h rs
  | otherwise         = Tape d ls h (c : rs)

reverseTape :: Tape -> Tape
reverseTape (Tape d ls c rs) = Tape d rs c ls

moveR :: Tape -> Tape
moveR = reverseTape . moveL . reverseTape

exec :: Program -> Exec ()
exec = foldM (const fold) () . runProgram

fold :: Tree Instruction -> Exec ()
fold (Leaf i) = execInstr i
fold (Node b) = while' (Program b)

while' :: Program -> Exec ()
while' b = undefined --do
--  x <- use cursor
--  when (x /= 0) (exec b >> while' b)

cursor :: Lens' Tape Set
cursor f (Tape d l c r) = (\x -> Tape d l x r) <$> f c

--data StateT s m a  = StateT  { unS :: s -> m (a, s) }
--data ListT m a     = ListT   { unL :: m (Maybe (a, ListT m a)) }
--data WriterT w m a = WriterT { unW :: m (a, w) }

type Exec = StateT Tape (ListT (WriterT [Set] Identity))

instance MonadWriter w m => MonadWriter w (ListT m) where
  tell = lift . tell
  listen = undefined
  pass = undefined

execInstr :: Instruction -> Exec ()
execInstr Inc   = undefined --cursor += 1
execInstr Dec   = undefined --cursor -= 1
execInstr In    = cursor .= Set 0 True
execInstr Out   = tell . (:[]) =<< use cursor
execInstr MoveL = modify moveL
execInstr MoveR = modify moveR
