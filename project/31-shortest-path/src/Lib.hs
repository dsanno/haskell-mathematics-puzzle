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

countPaths :: Int -> Int -> Int -> Int
countPaths w 1 0 = 2
countPaths w 1 returnY = 1
countPaths 1 h returnY
  | h == returnY = returnY
  | otherwise = returnY + 2
countPaths w h 0 = rightPathNum + downPathNum
  where
    rightPathNum = sum $ map (countPaths (w - 1) h) [1..h]
    downPathNum = sum $ map (countPaths (h - 1) w) [1..w]
countPaths w h returnY = rightPathNum + downPathNum
  where
    rightPathNum = sum $ map (countPaths (w - 1) h) [returnY..h]
    downPathNum = countPaths w (h - 1) (returnY - 1)

countUnoverwrappedPaths :: Int -> Int -> Int
countUnoverwrappedPaths w h = countPaths w h 0
