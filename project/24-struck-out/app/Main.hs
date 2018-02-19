module Main where

import qualified Data.Set as Set
import           Lib

main :: IO ()
main = do
  let ms = [ [1], [2], [3], [4], [5], [6], [7], [8], [9]
           , [1, 2], [2, 3], [3, 6], [6, 9], [9, 8], [8, 7], [7, 4], [4, 1]]
      n = countPatterns ms $ Set.fromList [1..9]
  print n
