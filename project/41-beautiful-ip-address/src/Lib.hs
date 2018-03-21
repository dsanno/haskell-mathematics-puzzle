module Lib
    ( intToBits
    , bitsToInt
    , symmetrize
    , makeSymmetricAddresses
    , makeUniqueSymmetricAddresses
    ) where

import qualified Data.List as List


intToBits :: Int -> Int -> [Int]
intToBits 0 _ = []
intToBits 1 n = [mod n 2]
intToBits len n = r : intToBits (len - 1) q
  where
    (q, r) = divMod n 2

bitsToInt :: [Int] -> Int
bitsToInt = List.foldr (\b n -> n * 2 + b) 0

makeSymmetricAddresses :: [[Int]]
makeSymmetricAddresses = map symmetrize ns
  where
    ns = [[x, y] | x <- [0..255], y <- [0..255]]

makeUniqueSymmetricAddresses :: [[Int]]
makeUniqueSymmetricAddresses = filter ((== "0123456789") . List.sort . intsToString) makeSymmetricAddresses

symmetrize :: [Int] -> [Int]
symmetrize ns = ns ++ reverse ms
  where
    ms = map (bitsToInt . reverse . intToBits 8) ns

intsToString :: [Int] -> String
intsToString = concatMap show
