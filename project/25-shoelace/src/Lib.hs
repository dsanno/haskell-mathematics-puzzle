module Lib
    ( isCross
    , countCross
    , paths
    ) where

import qualified Data.List as List
import qualified Data.Set  as Set

type Position = (Bool, Int)

isCross' :: Int -> Int -> Int -> Int -> Bool
isCross' a b c d
  | a > c && b < d = True
  | a < c && b > d = True
  | otherwise = False

isCross :: (Position, Position) -> (Position, Position)-> Bool
isCross ((False, a), (True, b)) ((False, c), (True, d)) = isCross' a b c d
isCross ((True, a), (False, b)) ((True, c), (False, d)) = isCross' a b c d
isCross ((False, a), (True, b)) ((True, c), (False, d)) = isCross' a b d c
isCross ((True, a), (False, b)) ((False, c), (True, d)) = isCross' a b d c
isCross _ _                                             = False

countCross :: [Position] -> Int
countCross xs =
  let ys = zip xs $ tail xs
      n = sum $ concatMap (\y -> map (fromEnum . isCross y) ys) ys
  in div n 2

paths' :: Set.Set Position -> Set.Set Position -> [[Position]]
paths' xs ys
  | Set.null xs = [[]]
  | otherwise = concatMap (\x -> map ((:) x) $ paths' ys $ Set.delete x xs) $ Set.toList xs

paths :: Int -> [[Position]]
paths n =
  let begin = (False, 0)
      end = (True, 0)
      xs = Set.fromList $ map ((,) True) [1..n - 1]
      ys = Set.fromList $ map ((,) False) [1..n - 1]
      ps = paths' xs ys
  in map ((:) begin . (++ [end])) ps
