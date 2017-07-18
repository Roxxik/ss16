module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import qualified BF
import BFSL3 hiding (IO)

main :: IO ()
main = print $ BF.execBF (C.pack "c") $ runOBFSL' $ inp >>> cast >>> outNumber
