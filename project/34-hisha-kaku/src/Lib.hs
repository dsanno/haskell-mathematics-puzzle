module Lib
    ( hishaMoves
    , kakuMoves
    , countMoves
    , makePositions
    ) where

import qualified Data.List as List
import qualified Data.Set  as Set


type Point = (Int, Int)


makePositions :: Int -> [(Point, Point)]
makePositions size = [ ((x1, y1), (x2, y2))
                     | x1 <- ids, y1 <- ids, x2 <- ids, y2 <- ids
                     , x1 /= x2 || y1 /= y2]
  where
    ids = [0..(size - 1)]

countMoves :: Int -> Point -> Point -> Int
countMoves size hishaPoint kakuPoint = Set.size $ Set.union hmoves kmoves
  where
    hmoves = hishaMoves size (Set.singleton kakuPoint) hishaPoint
    kmoves = kakuMoves size (Set.singleton hishaPoint) kakuPoint

hishaMoves :: Int -> Set.Set Point -> Point -> Set.Set Point
hishaMoves size pieces p = moves size pieces p [(1, 0), (-1, 0), (0, 1), (0, -1)]

kakuMoves :: Int -> Set.Set Point -> Point -> Set.Set Point
kakuMoves size pieces p = moves size pieces p [(1, 1), (1, -1), (-1, 1), (-1, -1)]

moves :: Int -> Set.Set Point -> Point -> [Point] -> Set.Set Point
moves size pieces p = Set.unions . map (Set.fromList . dirMoves size pieces p)

dirMoves :: Int -> Set.Set Point -> Point -> Point -> [Point]
dirMoves size pieces p dir =
  List.takeWhile (canMove size pieces) $ List.tail $ iterate (add dir) p

add :: Point -> Point -> Point
add (x, y) (x', y') = (x + x', y + y')

canMove :: Int -> Set.Set Point -> Point -> Bool
canMove size pieces p@(x, y)
  | x < 0 || x >= size || y < 0 || y >= size = False
  | Set.member p pieces = False
  | otherwise = True
