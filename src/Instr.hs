{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Instr where

import Tree

data Instruction = Inc | Dec | MoveL | MoveR | In | Out
  deriving (Eq, Show)

newtype Program = Program { runProgram :: Program' }
  deriving (Eq, Monoid)


instance Show Program where
  show = concatMap (flatten '[' ']'. fmap instToChar) . runProgram

type Program' = [Tree Instruction]


instToChar :: Instruction -> Char
instToChar Inc   = '+'
instToChar Dec   = '-'
instToChar MoveL = '<'
instToChar MoveR = '>'
instToChar Out   = '.'
instToChar In    = ','

emptyProgram :: Program'
emptyProgram = []


instr :: Instruction -> Program
instr i = Program [Leaf i]

while :: Program -> Program
while (Program bf) = Program [Node bf]

inc :: Program
inc = instr Inc

dec :: Program
dec = instr Dec

movL :: Program
movL = instr MoveL

movR :: Program
movR = instr MoveR

inp :: Program
inp = instr In

out :: Program
out = instr Out
