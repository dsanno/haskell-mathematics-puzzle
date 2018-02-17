module Lib
    ( countSubBySum
    ) where

import Data.Map.Lazy as Map

countSubBySum' :: Int -> [Int] -> Map.Map Int Int
countSubBySum' n [] = Map.empty
countSubBySum' n (m:ms) =
  let x = countSubBySum' n ms
      y = countSubBySum' (n + m) ms
  in Map.insertWith (+) (n + m) 1 $ Map.unionWith (+) x y

countSubBySum :: [Int] -> Map.Map Int Int
countSubBySum = countSubBySum' 0
