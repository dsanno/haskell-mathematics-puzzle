module Main where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

import qualified Lib


isSquare :: Int -> Bool
isSquare n = (round . sqrt . fromIntegral) n ^ 2 == n

findSquareNeighbors' :: Set.Set Int -> Int -> Int -> Maybe [Int]
findSquareNeighbors' ns firstNum lastNum =
  let ms = filter (isSquare . (+ lastNum)) $ Set.toList ns
      ys = Maybe.mapMaybe (\m -> findSquareNeighbors' (Set.delete m ns) firstNum m) ms
  in case ys of
    []    -> Nothing
    (x:_) -> Just x

findSquareNeighbors :: Int -> Maybe [Int]
findSquareNeighbors n = findSquareNeighbors' (Set.fromList [2..n]) 1 1

main :: IO ()
main = print . head $ Maybe.mapMaybe findSquareNeighbors [2..]
