module Main where

import Data.List as List
import Lib


main :: IO ()
main = do
  let rows = xorTriangle
      zeroNums = map (length . filter (== 0)) rows
      index = List.findIndex (>= 2014) $ scanl1 (+) zeroNums
  print $ (+ 1) <$> index
