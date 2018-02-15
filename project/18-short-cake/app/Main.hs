module Main where

import qualified Data.Maybe as Maybe
import qualified Data.Set   as Set

isSquare :: Int -> Bool
isSquare n = (round . sqrt . fromIntegral) n ^ 2 == n

findSquareNeighbors' :: Set.Set Int -> Int -> Int -> Maybe [Int]
findSquareNeighbors' ns firstNum lastNum
  | Set.null ns && isSquare (firstNum + lastNum) = Just [lastNum]
  | otherwise =
    let ms = filter (isSquare . (+ lastNum)) $ Set.toList ns
        ys = Maybe.mapMaybe (\m -> findSquareNeighbors' (Set.delete m ns) firstNum m) ms
    in case ys of
      []    -> Nothing
      (x:_) -> Just $ lastNum:x

findSquareNeighbors :: Int -> Maybe [Int]
findSquareNeighbors n =
  let ns = Set.fromList [2..n]
  in findSquareNeighbors' ns 1 1

main :: IO ()
main = do
  let xs = head $ Maybe.mapMaybe findSquareNeighbors [2..]
  print xs
  print $ length xs
