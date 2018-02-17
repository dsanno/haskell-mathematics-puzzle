module Main where

import qualified Data.List     as List
import qualified Data.Map.Lazy as Map
import           Lib

main :: IO ()
main = do
  let ns = countSubBySum [1, 14, 14, 4, 11, 7, 6, 9, 8, 10, 10, 5, 13, 2, 3, 15]
  print $ ns Map.! 33
  print $ Map.foldlWithKey (\(k', v') k v -> if v > v' then (k, v) else (k', v')) (0, 0) ns
