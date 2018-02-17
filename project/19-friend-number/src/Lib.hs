module Lib
    ( primes
    , factorize
    , friends
    ) where

import qualified Data.List as List
import qualified Data.Set as Set

primes :: [Int]
primes =
  f [2..]
  where f (p:ns) = p : f [n | n <- ns, n `mod` p /= 0]

factorize :: [Int] -> Int -> [Int]
factorize _ 1 = []
factorize ps n =
  case List.find ((== 0) . mod n) ps of
    Just m -> m : factorize ps (div n m)
    _      -> [n]

friends :: [Int] -> Set.Set [Int] -> Set.Set [Int]
friends ns = Set.filter (any (`List.elem` ns))
