module Lib
    ( swapCounts
    , expandSwapCounts
    , swap
    , pairs
    ) where

import           Data.Array      (Array, (!), (//))
import qualified Data.Array      as Array
import           Data.Ix         (Ix)
import           Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | make map that contains swap counts
--
-- >>> Map.foldl (+) 0 $ swapCounts 3
-- 7
-- >>> Map.size $ swapCounts 3
-- 6

swapCounts :: Int -> Map (Array Int Int) Int
swapCounts n = snd $ until (\(_, cs) -> Map.size cs >= maxSize) (\(c, cs) -> (c + 1, expandSwapCounts c cs)) (0, counts)
  where
    maxSize = List.product [2..n]
    counts = Map.singleton (Array.listArray (0, n - 1) [1..n]) 0


-- | expand map that contains swap counts
--
-- $setup
-- >>> x = Array.listArray (0,1) [1..2]
--
-- >>> expandSwapCounts 0 $ Map.singleton x 0
-- fromList [(array (0,1) [(0,1),(1,2)],0),(array (0,1) [(0,2),(1,1)],1)]

expandSwapCounts :: (Ix i, Eq e, Ord e) => Int -> Map (Array i e) Int -> Map (Array i e) Int
expandSwapCounts i counts = Map.foldlWithKey expandArray counts subCounts
  where
    subCounts = Map.filter (== i) counts
    expandArray cs a c = List.foldl (\cs' a' -> Map.insertWith min a' (c + 1) cs') cs $ swaps a


-- | swap each element pairs
--
-- $setup
-- >>> x = Array.listArray (0,2) [1..3]
--
-- >>> swaps x
-- [array (0,2) [(0,2),(1,1),(2,3)],array (0,2) [(0,3),(1,2),(2,1)],array (0,2) [(0,1),(1,3),(2,2)]]

swaps :: (Ix i, Eq e, Ord e) => Array i e -> [Array i e]
swaps a = map (\(i, j) -> swap i j a) indexPairs
  where
    indexPairs = pairs $ Array.indices a

-- | swap array elements
--
-- $setup
-- >>> x = Array.listArray (0,3) [1..4]
--
-- >>> swap 0 3 x
-- array (0,3) [(0,4),(1,2),(2,3),(3,1)]

swap :: (Ix i, Eq e, Ord e) => i -> i -> Array i e -> Array i e
swap i j a = a // [(i, a ! j),(j, a ! i)]

-- | make pairs from list
--
-- >>> pairs [1..3]
-- [(1,2),(1,3),(2,3)]

pairs :: (Ord a) => [a] -> [(a,a)]
pairs xs = [(a, b) | a <- xs, b <- xs, a < b]
