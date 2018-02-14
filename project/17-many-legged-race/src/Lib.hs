module Lib
    ( countRowPatterns
    ) where


countRowPatterns' :: Int -> (Int, Int)
countRowPatterns' 1 = (1, 1)
countRowPatterns' n =
  let (boyLeading, girlLeading) = countRowPatterns' (n - 1)
  in (boyLeading + girlLeading, boyLeading)

countRowPatterns :: Int -> Int
countRowPatterns n =
  let (boyLeading, girlLeading) = countRowPatterns' n
  in boyLeading + girlLeading
