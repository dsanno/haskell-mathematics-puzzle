module CutStick
( solve
) where

import qualified Data.List as List


cut :: Int -> Int -> Int
cut n i = min n (2 ^ i)

estimate :: Int -> Int -> Int
estimate len n =
  let xs = scanl (\s i -> s + cut n i) 0 [1..]
  in maybe (-1) (+ 1) $ List.findIndex (>= len - 1) xs

solve :: [Int]
solve = [ estimate 20 3
        , estimate 100 5
        ]
