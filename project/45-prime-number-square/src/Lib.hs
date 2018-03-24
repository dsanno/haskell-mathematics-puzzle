module Lib
    ( makeSquares
    , intToDigits
    , takeRange
    , primeNumbers
    ) where

import qualified Data.List as List


-- | make number squares from number lists
--
-- >>> List.sort $ makeSquares [[1,2],[1,3],[2,4],[2,5],[3,4]]
-- [[[1,2],[3,4]],[[1,3],[2,4]]]

makeSquares :: (Eq a) => [[a]] -> [[[a]]]
makeSquares rows = filter isSquare combinations
  where
    rowsWithHeads = map (map (`selectWithHead` rows)) rows
    combinations = List.concatMap makeCombinations rowsWithHeads
    isSquare rs = not (duplicates (rs ++ ts)) && all (`elem` rows) ts
                  where
                    ts = List.transpose rs

-- | select lists starts with passed element
--
-- >>> selectWithHead 2 [[1,2],[2,3],[1,3],[2,4]]
-- [[2,3],[2,4]]

selectWithHead :: (Eq a) => a -> [[a]] -> [[a]]
selectWithHead h = filter (\x -> head x == h)

-- | make combinations from list of lists
-- | take first elements from first list, second from second list, ...etc
--
-- >>> List.sort $ makeCombinations [[1,2],[3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
-- >>> List.sort $ makeCombinations [[1,2],[3],[4,5]]
-- [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]

makeCombinations :: (Eq a) => [[a]] -> [[a]]
makeCombinations = List.foldr (\x xs -> (:) <$> x <*> xs) [[]]

-- | tell if list has duplicate elements
--
-- >>> duplicates []
-- False
-- >>> duplicates [1,2,3]
-- False
-- >>> duplicates [1,2,3,1]
-- True

duplicates :: (Eq a) => [a] -> Bool
duplicates [] = False
duplicates [x] = False
duplicates (x:xs) = List.elem x xs || duplicates xs

-- | split int to digits
--
-- >>> intToDigits 100
-- [1,0,0]
-- >>> intToDigits 987
-- [9,8,7]

intToDigits :: Int -> [Int]
intToDigits = reverse . intToDigits'

intToDigits' :: Int -> [Int]
intToDigits' n
  | n < 0 = []
  | n < 10 = [n]
  | otherwise = r : intToDigits' q
  where
    (q, r) = divMod n 10

-- | take numbers within a range from sorted numbers
--
-- | >>> takeRange 10 12 [1..100]
-- | [10,11]
-- | >>> takeRange 98 102 [1..100]
-- | [98,99,100]
-- | >>> takeRange 20 23 [1..]
-- | [20,21,22]

takeRange :: Int -> Int -> [Int] -> [Int]
takeRange minNum upper xs = takeWhile (< upper) $ dropWhile (< minNum) xs

-- | prime numbers
--
-- >>> take 10 primeNumbers
-- [2,3,5,7,11,13,17,19,23,29]

primeNumbers :: [Int]
primeNumbers = selectPrimes [2..]

-- | select prime numbers
--
-- >>> selectPrimes [2,3,4,5,6]
-- [2,3,5]

selectPrimes :: [Int] -> [Int]
selectPrimes [] = []
selectPrimes (x:xs) = x : filter ((/= 0) . (`mod` x)) (selectPrimes xs)
