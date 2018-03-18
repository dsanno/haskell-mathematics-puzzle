module Main where

import Lib

main :: IO ()
main = do
  let (bits, n) = findMaxMove
  putStrLn "number of moves:"
  print n
  putStrLn "bits"
  printBits bits
