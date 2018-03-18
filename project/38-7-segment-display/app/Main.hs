module Main where

import Lib

main :: IO ()
main = do
  let result = findMinimumSwitches [0..9]
  print result
  print $ countSwitches result
