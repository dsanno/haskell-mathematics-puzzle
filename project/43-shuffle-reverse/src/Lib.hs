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
minShuffleCount n = evalState (minShuffleCountWithCache size offsets target [[1..n]]) Set.empty
  where
    size = div n 2
    offsets = [1..(n - size)]
    target = reverse [1..n]

minShuffleCountWithCache :: (Ord a) => Int -> [Int] -> [a] -> [[a]] -> State (Set [a]) Int
minShuffleCountWithCache size offsets target xs =
  if target `elem` ys
  then return 1
  else do
    cache <- get
    let c = Set.fromList ys
        nexts = Set.toList $ Set.difference c cache
    put $ Set.union c cache
    count <- minShuffleCountWithCache size offsets target nexts
    return $ count + 1
  where
    ys = shuffle size <$> offsets <*> xs


-- | find minimum Shuffle Count
--
-- >>> minShuffleCount' 2
-- Just 1
-- >>> minShuffleCount' 4
-- Just 4

minShuffleCount' :: Int -> Maybe Int
minShuffleCount' n = List.findIndex (target `elem`) steps
  where
    size = div n 2
    target = reverse [1..n]
    step lists = shuffle size <$> [1..(n - size)] <*> lists
    steps = iterate step [[1..n]]

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
