module Number
( digits
, Number.reverse
, isPalindrome
) where

digits :: Int -> Int -> [Int]
digits b n = Prelude.reverse $ digits' b n

digits' :: Int -> Int -> [Int]
digits' b n
  | n < b = [n]
  | otherwise = rem n b : digits' b (div n b)

reverse :: Int -> Int
reverse n = foldl (\s x -> s * 10 + x) 0 $ digits' 10 n

isPalindrome :: Int -> Int -> Bool
isPalindrome b n
  | b > 0 =
    let xs = digits' b n
    in xs == Prelude.reverse xs
  | otherwise = False
