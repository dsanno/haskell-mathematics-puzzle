module Lib
    ( xorTriangle
    ) where

import Data.Bits (xor)


xorRow :: [Int] -> [Int]
xorRow xs = 1 : zipWith xor xs (tail xs) ++ [1]

xorTriangle :: [[Int]]
xorTriangle = iterate xorRow [1]
