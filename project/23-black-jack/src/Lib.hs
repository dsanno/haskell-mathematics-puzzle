module Lib
    ( countAlive
    ) where

countAlive :: Int -> Int -> Int
countAlive amount 1
  | amount < 1  = 0
  | amount == 1 = 1
  | otherwise   = 2
countAlive amount count
  | amount <= 0 = 0
  | otherwise =
    countAlive (amount - 1) (count - 1) + countAlive (amount + 1) (count - 1)
