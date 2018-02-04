module Flip
( solve
) where


xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

isOpen :: Int -> Bool
isOpen n = foldl (\b i -> xor b (mod n i == 0)) False [2..n]

solve :: [Int]
solve = filter (not . isOpen) [1..100]
