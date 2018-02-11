module SquareRoot
( solve
) where

import           Data.List (find, nub)
import           Text.Printf (printf)


takeDecimal :: String -> String
takeDecimal = dropWhile (/= '.')

formatFloat :: Double -> String
formatFloat = printf "%f"

solve :: (Maybe Int, Maybe Int)
solve =
  let x = find ((== 11) . length . nub . take 11 . formatFloat . sqrt) [1..]
      y = find ((== 11) . length . nub . take 11 . takeDecimal . formatFloat . sqrt) [1..]
  in (round <$> x, round <$> y)
