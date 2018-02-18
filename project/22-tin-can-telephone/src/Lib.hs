module Lib
    ( countPatterns
    ) where

countPatterns :: Int -> Int
countPatterns 0 = 0
countPatterns 2 = 1
countPatterns n
  | odd n = 0
  | otherwise =
      let x = countPatterns (n - 2) * 2
          y = sum $ map (\m -> countPatterns m * countPatterns (n - m - 2)) [2, 4..(n - 4)]
      in x + y
