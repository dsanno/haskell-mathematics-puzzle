module Main where

import Lib

main :: IO ()
main = do
  let addresses = makeUniqueSymmetricAddresses
  print addresses
  print $ length addresses
