module Lib
    ( orderdCombinations
    , countPatterns
    ) where

import qualified Data.List           as List


orderdCombinations :: Int -> Int -> Int -> [[Int]]
orderdCombinations 1 minNum total
  | total >= minNum && total >= 1 = [[total]]
  | otherwise = []
orderdCombinations size minNum total =
  List.concatMap (\n -> [(n:)] <*> orderdCombinations (size - 1) n (total - n)) [minNum..(div total size)]

countCombinations2 :: (Int -> Int) -> [Int] -> Int
countCombinations2 f [a, b]
  | a == b = fa * (fa + 1) `div` 2
  | otherwise = fa * fb
  where
    fa = f a
    fb = f b

countCombinations3 :: (Int -> Int) -> [Int] -> Int
countCombinations3 f [a, b, c]
  | a == b && b == c = fa * (fa + 1) * (fa + 2) `div` 6
  | a == b           = fa * (fa + 1) * fc `div` 2
  | b == c           = fa * fb * (fb + 1) `div` 2
  | a == c           = fa * (fa + 1) * fb `div` 2
  | otherwise = fa * fb * fc
  where
    fa = f a
    fb = f b
    fc = f c

countPatterns :: Int -> Int
countPatterns n
  | n <= 0 = 0
  | n == 1 = 1
  | n == 2 = 1
  | otherwise =
      sum (map (countCombinations2 countPatterns) c2) +
      sum (map (countCombinations3 countPatterns) c3)
      where
        c2 = orderdCombinations 2 1 n
        c3 = orderdCombinations 3 1 n
