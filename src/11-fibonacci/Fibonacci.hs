module Fibonacci
( solve
) where

import Number


fibs :: [Int]
fibs = map fst $ iterate (\(x, y) -> (y, x + y)) (1, 1)

sumDigits :: Int -> Int
sumDigits = sum . Number.digits 10

canDivideByDigitsSum :: Int -> Bool
canDivideByDigitsSum n = mod n (sumDigits n) == 0

solve :: [Int]
solve =
  drop 8 $ take 13 $ filter canDivideByDigitsSum fibs
