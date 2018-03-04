module Lib
    ( searchMinimumPath
    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map.Lazy       as Map
import qualified Data.Maybe          as Maybe

type Point = (Int, Int)
type Size = (Int, Int)
type Position = (Point, Point)
type Cache = Map.Map Position [Point]

isIn :: Size -> Point -> Bool
isIn (w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move :: Size -> Position -> Point -> Maybe Position
move size (target, empty) m
  | add empty m == target = Just (empty, target)
  | isIn size $ add empty m = Just (target, add empty m)
  | otherwise = Nothing

search :: Size -> Point -> [Position] -> State Cache (Maybe [Point])
search _ _ [] = return Nothing
search size goal positions =
  case List.find ((== goal) . fst) positions of
    Just position -> do
      cache <- get
      return $ Map.lookup position cache
    _ -> do
      let pairs = (,) <$> positions <*> [(1, 0), (-1, 0), (0, 1), (0, -1)]
      nextPositions <- foldM (\xs (p, m) -> do
                               cache <- get
                               let path = cache Map.! p
                               case move size p m of
                                 Just nextPos@(_, e)
                                   | Map.notMember nextPos cache -> do
                                       modify $ Map.insert nextPos (e:path)
                                       return (nextPos:xs)
                                   | otherwise -> return xs
                                 _ -> return xs
                             ) [] pairs
      search size goal nextPositions

searchMinimumPath :: Size -> Point -> Position -> Maybe [Point]
searchMinimumPath size goal p = evalState (search size goal [p]) $ Map.singleton p []
