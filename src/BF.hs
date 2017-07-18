{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module BF where

import Data.Maybe
import Data.List
import Data.Word

import Control.Monad.Free.Church
import Control.Monad.RWS

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Tree
import Util
import Lens
import Next
import Instr

data FlatInstruction = FBasic Instruction | If | Loop
  deriving (Eq)

instance Show FlatInstruction where
  show = show . instToChar'

data Tape = Tape ByteString Word8 ByteString

instance Show Tape where
  show (Tape l x r) =
      "["
    ++ (intercalate "," . map show . B.unpack $ C.reverse l)
    ++ ">"
    ++ show x
    ++ "<"
    ++ (intercalate "," . map show . B.unpack $ r)
    ++ "]"

emptyTape :: Tape
emptyTape = Tape B.empty 0 B.empty

instToChar' :: FlatInstruction -> Char
instToChar' (FBasic x) = instToChar x
instToChar' If         = '['
instToChar' Loop       = ']'

charToInst :: Char -> Maybe FlatInstruction
charToInst '+' = Just . FBasic $ Inc
charToInst '-' = Just . FBasic $ Dec
charToInst '<' = Just . FBasic $ MoveL
charToInst '>' = Just . FBasic $ MoveR
charToInst '.' = Just . FBasic $ Out
charToInst ',' = Just . FBasic $ In
charToInst '[' = Just If
charToInst ']' = Just Loop
charToInst _   = Nothing

purge :: Instruction -> Instruction -> Bool
purge Inc   Dec   = True
purge Dec   Inc   = True
purge MoveL MoveR = True
purge MoveR MoveL = True
purge _     _     = False

-- this one is ugly
opt2 :: (Instruction -> Instruction -> Bool) -> Program' -> Program'
opt2 f [] = []
opt2 f (Node [Node ts] : is) = Node (opt2 f ts) : opt2 f is
opt2 f (Node ts : is) = Node (opt2 f ts) : opt2 f is
opt2 f (Leaf i : Leaf j : is)
  | f i j = opt2 f is
  | otherwise = Leaf i : opt2 f (Leaf j : is)
opt2 f (Leaf i : x : is) = Leaf i : opt2 f (x : is)
opt2 f is = is

optimize :: Program -> Program
optimize = Program . opt2 purge . runProgram

optimize2 :: Program -> Program
optimize2 x = let y = optimize x in if x == y then x else optimize2 y

parse :: String -> Maybe Program
parse = ast . parse'

parse' :: String -> [FlatInstruction]
parse' = mapMaybe charToInst

ast :: [FlatInstruction] -> Maybe Program
ast = fmap Program . unflattenForest delim
  where
  delim If = Start
  delim Loop = End
  delim (FBasic i) = Delimited i



run = execBF (C.pack "")

execBF :: ByteString -> Program -> (ByteString, Tape, ByteString)
execBF i = runExec i . exec

runExec :: ByteString -> RWST () ByteString Tape Next () -> (ByteString, Tape, ByteString)
runExec i e = (\(((),t,o),i) -> (i,t,o)) $ runNext (runRWST e () emptyTape) i

exec :: (MonadState Tape m, MonadNext Word8 m, MonadWriter ByteString m) => Program -> m ()
exec = foldM (const fold) () . runProgram

fold :: (MonadState Tape m, MonadNext Word8 m, MonadWriter ByteString m) => Tree Instruction -> m ()
fold (Leaf i) = execInstr i
fold (Node b) = while' (Program b)

while' :: (MonadState Tape m, MonadNext Word8 m, MonadWriter ByteString m) => Program -> m ()
while' b = do
  x <- use cursor
  when (x /= 0) (exec b >> while' b)

cursor :: Lens' Tape Word8
cursor f (Tape l c r) = (\x -> Tape l x r) <$> f c

execInstr :: (MonadState Tape m, MonadNext Word8 m, MonadWriter ByteString m) => Instruction -> m ()
execInstr Inc   = cursor += 1
execInstr Dec   = cursor -= 1
execInstr In    = cursor <~ (fromMaybe 0 <$> next)
execInstr Out   = tell . B.singleton =<< use cursor
execInstr MoveL = modify moveL
execInstr MoveR = modify moveR

moveL :: Tape -> Tape
moveL = putHead . getHead

getHead :: Tape -> (Word8, Tape)
getHead t@(Tape ls c rs) = case B.uncons ls of
  Nothing     -> (0, t)
  Just (x,xs) -> (x, Tape xs c rs)

putHead :: (Word8, Tape) -> Tape
putHead (h, Tape ls c rs)
  | c == 0 && B.null rs = Tape ls h rs
  | otherwise           = Tape ls h (B.cons c rs)

reverseTape :: Tape -> Tape
reverseTape (Tape ls c rs) = Tape rs c ls

moveR :: Tape -> Tape
moveR = reverseTape . moveL . reverseTape
