module Main where

import qualified Data.List as List
import qualified Data.Set as Set

import Lib

update :: Set.Set [Int] -> Set.Set [Int] -> [Int] -> (Set.Set [Int], Set.Set [Int])
update loopSet noLoopSet x
  | Set.member x loopSet || Set.member x noLoopSet = (loopSet, noLoopSet)
  | otherwise = (foldr Set.insert loopSet loop, foldr Set.insert noLoopSet noLoop)
  where
    (noLoop, loop) = splitLoop x

main :: IO ()
main = do
  let (loopSet, noLoopSet) = foldl (\(ls, nls) x -> update ls nls x) (Set.empty, Set.empty) (makeDicePatterns 6)
  print $ Set.size noLoopSet
