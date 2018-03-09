module Lib
    ( distribute
    ) where

import           Data.Function (on)
import qualified Data.List     as List


type Club = (Int, Int)

sumClub :: [Club] -> Club
sumClub clubs = (sum areas, sum members)
  where (areas, members) = unzip clubs

totalAreaWithMaxMembers :: Int -> [Club] -> [Bool] -> Int
totalAreaWithMaxMembers maxMembers clubs flags
  | m <= maxMembers = a
  | otherwise = 0
  where (a, m) = sumClub . map fst . filter snd $ zip clubs flags

distribute :: Int -> [Club] -> [Bool]
distribute maxMember clubs =
  List.maximumBy compareFlag patterns
  where
    flags = replicate (length clubs) [True, False]
    patterns = foldr (\x xs -> (:) <$> x <*> xs) [[]] flags
    compareFlag = on compare $ totalAreaWithMaxMembers maxMember clubs
