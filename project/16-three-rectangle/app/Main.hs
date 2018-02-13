module Main where

import Lib


rejectReducible :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
rejectReducible = filter (\((a, b), (c, d)) -> foldl1 gcd [a, b, c, d] == 1)

main :: IO ()
main = do
  let rectanglePairs = concatMap (rejectReducible . Lib.rectanglePairs) [4, 8..500]
  putStrLn "rectanbles:"
  print rectanglePairs
  putStrLn "total number:"
  print $ length rectanglePairs
