module Main where

import           Lib

main :: IO ()
main = do
  let size = (10, 10)
      goal = (9, 9)
      p = ((0, 0), (9, 9))

  let path = searchMinimumPath size goal p
  putStrLn "minimum path:"
  print path
  putStrLn "path length"
  print $ length <$> path
