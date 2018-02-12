module NameChain
( solve
) where

import           Control.Arrow ((&&&))
import qualified Data.List     as List


names = [ "brazil", "croatia", "mexico", "cameroon", "spain", "netherlands"
        , "chile", "australia", "colombia", "greece", "cote d'ivoire"
        , "columbia", "uruguay", "costa rica", "england", "italy", "switzerland"
        , "ecuador", "france", "honduras", "argentina", "bosnia and herzegovina"
        , "iran", "nigeria", "germany", "portugal", "ghana", "usa", "belgium"
        , "algeria", "russia", "korea republic"
        ]

toHeadLastPairs :: [String] -> [(Char, Char)]
toHeadLastPairs = map $ head &&& last

maxChain' :: (Char, Char) -> [(Char, Char)] -> Int
maxChain' _ [] = 0
maxChain' (_, c) xs =
  let ys = filter ((== c) . fst) xs
  in case ys of
    [] -> 0
    _  ->
      let n = maximum . map (\y -> maxChain' y (List.delete y xs)) $ ys
      in n + 1

maxChain :: [String] -> Int
maxChain xs =
  let ys = toHeadLastPairs xs
      n = maximum . map (\y -> maxChain' y (List.delete y ys)) $ ys
  in n + 1

solve :: Int
solve = maxChain names
