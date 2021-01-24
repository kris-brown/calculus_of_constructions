module Main where

import CoC.Base (base)
import CoC.Checker (writeFile)
import Prelude hiding (writeFile)

main :: IO ()
main = writeFile base "../data/base2.txt"
