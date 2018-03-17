module Main where

import Lib

main :: IO ()
main = print . sum . map (uncurry (countMoves size)) $ positions
  where
    size = 9
    positions = makePositions size
