module Collatz
( solve
) where

import qualified Data.List as List
import qualified Data.Set as Set


next :: Int -> Int
next n
  | even n = div n 2
  | otherwise = 3 * n + 1

accumulateSet :: Ord a => [a] -> [Set.Set a]
accumulateSet = scanl (flip Set.insert) Set.empty

willReturn :: Int -> Bool
willReturn n =
  let ns = iterate next $ n * 3 + 1
      sets = accumulateSet $ n : ns
      m = fst <$> List.find (uncurry Set.member) (zip ns sets)
  in m == Just n

solve :: Int
solve = length $ filter id $ map willReturn [2,4..10000]
