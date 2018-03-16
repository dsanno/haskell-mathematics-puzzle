module Lib
    ( loadCsv
    , decisionNumber
    , decisionNumbers
    ) where

import qualified Data.List         as List
import qualified Text.CSV          as CSV
import           Text.Parsec.Error (ParseError)

loadCsv :: String -> IO (Either ParseError CSV.CSV)
loadCsv = CSV.parseCSVFromFile


decisionNumber :: String -> String -> Int
decisionNumber x y =
  case List.findIndex (uncurry (/=)) $ zip x y of
    Just n -> n + 1
    _      -> min (length x) (length y) + 1

decisionNumbers :: [String] -> [Int]
decisionNumbers [] = []
decisionNumbers [x] = [1]
decisionNumbers (x:xs) = maximum ms:zipWith max ns ms
  where
    ns = decisionNumbers xs
    ms = map (decisionNumber x) xs
