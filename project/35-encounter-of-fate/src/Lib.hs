module Lib
    ( makePaths
    , countDestinyPaths
    ) where

type Point = (Int, Int)
type Path = [Point]


countDestinyPaths :: Int -> Int -> Int
countDestinyPaths w h = sum . map fromEnum $ isDestiny <$> paths <*> reversePaths
  where
    paths = makePaths w h
    reversePaths = map reverse paths

isDestiny :: Path -> Path -> Bool
isDestiny x y = hasSamePoint x y || countOnSameLine x y >= 2

makePaths' :: Point -> Int -> Int -> [Path]
makePaths' p@(x, y) w h
  | x == w = [[(x, a) | a <- [y..h]]]
  | y == h = [[(a, y) | a <- [x..w]]]
  | otherwise = map (p:) $ makePaths' (x + 1, y) w h ++ makePaths' (x, y + 1) w h

makePaths :: Int -> Int -> [Path]
makePaths = makePaths' (0, 0)

hasSamePoint :: Path -> Path -> Bool
hasSamePoint x y = any (uncurry (==)) $ zip x y

countOnSameLine :: Path -> Path -> Int
countOnSameLine x y = sum . map (\((a, b), (a', b')) -> fromEnum (a == a' || b == b')) $ zip x y
