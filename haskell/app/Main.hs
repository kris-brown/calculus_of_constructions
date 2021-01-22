module Main where

import Base (base)
import Checker (writeFile)
import Prelude hiding (writeFile)

main :: IO ()
main = writeFile base "../data/base2.txt"
