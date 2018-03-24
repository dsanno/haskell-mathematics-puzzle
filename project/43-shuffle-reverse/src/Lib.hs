module Lib
    ( shuffle
    , minShuffleCount
    ) where

import           Control.Monad.State
import qualified Data.List           as List
import           Data.Set            (Set)
import qualified Data.Set            as Set


-- | find minimum Shuffle Count
--
-- >>> minShuffleCount 2
-- 1
-- >>> minShuffleCount 4
-- 4

minShuffleCount :: Int -> Int
minShuffleCount n = evalState (minShuffleCountWithCache size offsets targets [[1..n]]) (Set.empty, Set.empty)
  where
    size = div n 2
    offsets = [1..(n - size)]
    targets = [reverse [1..n]]

minShuffleCountWithCache :: (Ord a) => Int -> [Int] -> [[a]] -> [[a]] -> State (Set [a], Set [a]) Int
minShuffleCountWithCache size offsets targets xs
  | any (`Set.member` s1) targets = return 1
  | any (`Set.member` s2) s1 = return 2
  | otherwise = do
      (c1, c2) <- get
      let n1 = Set.difference s1 c1
      let n2 = Set.difference s2 c2
      put (Set.union c1 n1, Set.union c2 n2)
      count <- minShuffleCountWithCache size offsets (Set.toList n2) (Set.toList n1)
      return $ count + 2
  where
    ys1 = shuffle size <$> offsets <*> xs
    ys2 = reverseShuffle size <$> offsets <*> targets
    s1 = Set.fromList ys1
    s2 = Set.fromList ys2

-- | shuffle list
--
-- >>> shuffle 1 0 [1,2]
-- [1,2]
-- >>> shuffle 2 1 [1..4]
-- [2,3,1,4]

shuffle :: Int -> Int -> [a] -> [a]
shuffle size offset xs = b ++ a ++ c
  where
    (a, rest) = List.splitAt offset xs
    (b, c) = List.splitAt size rest

-- | reverse shuffle list
--
-- >>> reverseShuffle 1 0 [1, 2]
-- [1,2]
-- >>> reverseShuffle 2 1 [1..4]
-- [3,1,2,4]

reverseShuffle :: Int -> Int -> [a] -> [a]
reverseShuffle size offset xs = b ++ a ++ c
  where
    (a, rest) = List.splitAt size xs
    (b, c) = List.splitAt offset rest
