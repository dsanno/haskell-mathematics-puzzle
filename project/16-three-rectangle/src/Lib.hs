module Lib
    ( rectangles
    , pairs
    , rectanglePairs
    ) where

rectangles :: Int -> [(Int, Int)]
rectangles n
  | even n = [(i, n `div` 2 - i) | i <- [1..(n `div` 4)]]
  | otherwise = []

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) (x:xs) ++ pairs xs

rectanglePairs :: Int -> [((Int, Int), (Int, Int))]
rectanglePairs n
  | mod n 4 /= 0 = []
  | otherwise =
    let squareArea = n * n `div` 16
    in [((a, b), (c, d)) | ((a, b), (c, d)) <- (pairs . rectangles) n
       , a * b + c * d == squareArea]
