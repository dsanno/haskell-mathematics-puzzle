module Main where

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map

import Lib

main :: IO ()
main = do
  let counts = swapCounts 7
      cs = map (\(a, c) -> (Array.elems a, c)) $ Map.toList counts
  print cs
  print $ Map.foldl (+) 0 counts
