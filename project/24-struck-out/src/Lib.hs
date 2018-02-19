module Lib
    ( countPatterns
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

countPatterns' :: [[Int]] -> Set.Set Int -> Map.Map (Set.Set Int) Int -> (Int, Map.Map (Set.Set Int) Int)
countPatterns' ms ns cache
  | Set.null ns = (1, cache)
  | otherwise =
    case Map.lookup ns cache of
      Just count -> (count, cache)
      _ -> foldl (\(s, c) m ->
                   if all (`Set.member` ns) m
                     then
                       let ns' = foldr Set.delete ns m
                           (s', c') = countPatterns' ms ns' c
                       in (s + s', Map.insert ns' s' c)
                     else (s, c)
                 ) (0, cache) ms


countPatterns :: [[Int]] -> Set.Set Int -> Int
countPatterns ms ns = fst $ countPatterns' ms ns Map.empty
