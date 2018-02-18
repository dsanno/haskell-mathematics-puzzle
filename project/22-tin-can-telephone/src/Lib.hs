module Lib
    ( countPatterns
    ) where

import           Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map


countPatterns' :: Map.Map Int Int -> Int -> Int
countPatterns' ps n =
  sum $ map (\m -> ps ! m * ps ! (n - m - 2)) [0, 2..(n - 2)]

countPatterns :: Int -> Int
countPatterns 0 = 0
countPatterns n
  | odd n = 0
  | otherwise =
      let ps = foldl (\qs m -> Map.insert m (countPatterns' qs m) qs) (Map.singleton 0 1) [2, 4..n]
      in ps ! n
