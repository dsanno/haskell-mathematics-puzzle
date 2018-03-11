module Lib
    ( allPaths
    , countUnoverwrappedPaths
    ) where

type Point = (Int, Int)
type Segment = (Point, Point)
type Path = [Segment]

allPaths' :: Point -> Point -> [Path]
allPaths' from@(fx, fy) to@(tx, ty)
  | fx > tx || fy > ty = []
  | from == to = [[]]
  | fx == tx = downPaths
  | fy == ty = rightPaths
  | otherwise = rightPaths ++ downPaths
  where
    right = (fx + 1, fy)
    down = (fx, fy + 1)
    rightPaths = (:) <$> [(from, right)] <*> allPaths' right to
    downPaths = (:) <$> [(from, down)] <*> allPaths' down to

allPaths :: Int -> Int -> [Path]
allPaths w h = allPaths' (0, 0) (w, h)

overwrap :: Path -> Path -> Bool
overwrap xs ys = any (uncurry (==)) $ zip xs ys

countUnoverwrappedPaths :: Int -> Int -> Int
countUnoverwrappedPaths w h =
  sum $ map (`countUnoverwrapped` paths) paths
  where
    paths = allPaths w h
    countUnoverwrapped p ps = sum $ map (fromEnum . not . overwrap p) ps
