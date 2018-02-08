module RobotCleaner
( solve
) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe


newtype History = History [(Int, Int)]

initialHistory :: History
initialHistory = History [(0, 0)]

forward :: (Int, Int) -> History -> Maybe History
forward (a, b) (History []) = Just $ History [(a, b)]
forward (a, b) (History h@((x, y):_))
  | (x + a, y + b) `elem` h = Nothing
  | otherwise = Just $ History $ (x + a, y + b):h

expand :: [(Int, Int)] -> [History] -> [History]
expand ms hs = Maybe.catMaybes $ forward <$> ms <*> hs

findPaths :: Int -> [History]
findPaths n =
  let steps = [(0, 1), (1, 0), (0, -1), (-1, 0)]
  in iterate (expand steps) [initialHistory] !! n

solve :: Int
solve = length $ findPaths 12
