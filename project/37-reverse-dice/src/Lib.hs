module Lib
    ( step
    , splitLoop
    , makeDicePatterns
    ) where

import qualified Data.List as List
import qualified Data.Tuple as Tuple


step :: [Int] -> [Int]
step xs@(x:_) = bs ++ reverseDice as
  where
    (as, bs) = List.splitAt x xs

splitLoop :: [Int] -> ([[Int]], [[Int]])
splitLoop x = (reverse notLoop, reverse loop)
  where
    (notLoop, loop) = splitLoopWithHistory [] x

splitLoopWithHistory :: [[Int]] -> [Int] -> ([[Int]], [[Int]])
splitLoopWithHistory history x =
  case List.elemIndex x history of
    Just n -> Tuple.swap $ List.splitAt (n + 1) history
    _ -> splitLoopWithHistory (x:history) (step x)

makeDicePatterns :: Int -> [[Int]]
makeDicePatterns 1 = [[x] | x <- [1..6]]
makeDicePatterns n = (:) <$> [1..6] <*> makeDicePatterns (n - 1)

reverseDice :: [Int] -> [Int]
reverseDice = map (7 -)
