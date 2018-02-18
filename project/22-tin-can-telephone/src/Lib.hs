module Lib
    ( countPatterns
    ) where

import           Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map


countPatterns' :: Map.Map Int Int -> Int -> Int
countPatterns' ps n =
  let x = ps ! (n - 2) * 2
      y = sum $ map (\m -> ps ! m * ps ! (n - m - 2)) [2, 4..(n - 4)]
  in x + y

countPatterns :: Int -> Int
countPatterns 0 = 0
countPatterns 2 = 1
countPatterns n
  | odd n = 0
  | otherwise =
      let ps = foldl (\qs m -> Map.insert m (countPatterns' qs m) qs) (Map.singleton 2 1) [4, 6..n]
      in ps ! n
