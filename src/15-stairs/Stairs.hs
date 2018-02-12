module Stairs
( solve
) where

import           Control.Applicative ((<$>))
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe          as Maybe

singleMoves' :: Int -> Int -> Int -> Map.Map Int [[Int]] -> Map.Map Int [[Int]]
singleMoves' minStep maxStep total moveMap =
  let moves = Maybe.catMaybes [map (i:) <$> Map.lookup (total - i) moveMap | i <- [minStep..(min maxStep total)]]
  in Map.insert total (concat moves) moveMap

singleMoves :: Int -> Int -> Int -> [[Int]]
singleMoves minStep maxStep total =
  let moveMap = foldl (flip $ singleMoves' minStep maxStep) (Map.fromList [(0, [[]])]) [minStep..total]
  in moveMap Map.! total

applySubMoves :: Map.Map Int [(Int, Int)] -> [Int] -> [[(Int, Int)]]
applySubMoves moves =
  foldr ((\x xs -> (:) <$> x <*> xs) . (moves Map.!)) [[]]

doubleMoves :: Int -> Int -> Int -> [[(Int, Int)]]
doubleMoves minStep maxStep total =
  let subMoves = Map.fromList [(i, [(j, i - j) | j <- [minStep..maxStep], i - j >= minStep && i - j <= maxStep]) | i <- [(minStep * 2)..(maxStep * 2)]]
      ss = singleMoves (minStep * 2) (maxStep * 2) total
  in concatMap (applySubMoves subMoves) ss

solve :: Int
solve = length $ doubleMoves 1 4 10
