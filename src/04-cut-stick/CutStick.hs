module CutStick
( solve
) where

import qualified Data.List as List

{-
count :: Int -> Int -> Int
count len n =
  let f (i, count) = (i + 1, count + min n (2 ^ i))
      x = List.find ((>= len - 1) . snd) $ iterate f (0, 0)
  in case x of
       Just (i, _) -> i
       _           -> -1
-}

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
