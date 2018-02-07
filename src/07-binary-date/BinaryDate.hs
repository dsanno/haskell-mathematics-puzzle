module BinaryDate
( solve
) where

import           Data.Time
import qualified Number

isPalindrome :: Day -> Bool
isPalindrome d =
  let n = read $ formatTime defaultTimeLocale "%Y%m%d" d :: Int
  in Number.isPalindrome 2 n

findPalindromeDates :: Day -> Day -> [Day]
findPalindromeDates beginDate endDate =
  let diff = fromInteger $ diffDays endDate beginDate + 1
      dates = take diff $ iterate (addDays 1) beginDate
  in foldr (\d ds -> if isPalindrome d then d:ds else ds) [] dates

solve :: [String]
solve =
  let beginDay = fromGregorian 1964 10 10
      endDay = fromGregorian 2020 7 24
      dates = findPalindromeDates beginDay endDay
  in map (formatTime defaultTimeLocale "%Y-%m-%d") dates
