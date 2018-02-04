module PalindromeNumber
( solve
, isPalindromeNumber
) where

digits :: Int -> Int -> [Int]
digits b n
  | n < b = [n]
  | otherwise = rem n b : digits b (div n b)

isPalindromeNumber :: Int -> Int -> Bool
isPalindromeNumber b n
  | b > 0 =
    let xs = digits b n
    in xs == reverse xs
  | otherwise = False

solve :: Int
solve =
  head $ filter (\x ->
                  isPalindromeNumber 2 x &&
                  isPalindromeNumber 8 x &&
                  isPalindromeNumber 10 x
                ) [10..]
