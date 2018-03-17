module Lib
    ( isPalindrome
    , intToList
    , toBinary
    ) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

intToList' :: Int -> [Int]
intToList' n
  | n < 0 = []
  | n < 10 = [n]
  | otherwise = r:intToList' d
      where (d, r) = divMod n 10

intToList :: Int -> [Int]
intToList = reverse . intToList'

toBinary :: Int -> Int
toBinary n
  | n <= 0 = 0
  | otherwise = toBinary d * 10 + r
  where
    (d, r) = divMod n 2
