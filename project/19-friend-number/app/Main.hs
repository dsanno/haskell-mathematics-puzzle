module Main where

import qualified Data.List as List
import qualified Data.Set  as Set
import           Lib

maximumByLength :: [[a]] -> [a]
maximumByLength = List.maximumBy (\x y -> compare (length x) (length y))

friendsPath :: Set.Set [Int] -> [Int] -> [[Int]]
friendsPath ns m
  | Set.null ns = [m]
  | otherwise =
    let fs = friends m ns
        remain = Set.difference ns fs
        paths = Set.toList $ Set.map (friendsPath remain) fs
    in case paths of
         [] -> [m]
         _  -> m : maximumByLength paths

maxLengthPath :: Int -> [Int]
maxLengthPath n =
  let ps = takeWhile (< n) primes
      ss = Set.difference (Set.fromList [2..n]) $ Set.fromList ps
      ms = Set.map (factorize ps) ss
      factorPath = maximumByLength . Set.toList $ Set.map (\x -> friendsPath (Set.delete x ms) x) ms
  in case factorPath of
    [] -> []
    _  -> List.map List.product factorPath

main :: IO ()
main = do
  let path = List.find ((>= 7) . length) $ map maxLengthPath [2..]
  print path
  print $ List.maximum <$> path
