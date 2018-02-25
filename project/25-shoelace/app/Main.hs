module Main where

import qualified Data.List as List
import           Lib

main :: IO ()
main = do
  let ps = paths 6
      maxCrossPath = List.maximumBy (\x y -> compare (countCross x) (countCross y)) ps
  putStrLn "path:"
  print maxCrossPath
  putStrLn "cross count:"
  print $ countCross maxCrossPath
