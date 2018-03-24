module Main where

import Lib

main :: IO ()
main = do
  let primes = takeRange 100 1000 primeNumbers
      squares = makeSquares $ map intToDigits primes
  print $ length squares
