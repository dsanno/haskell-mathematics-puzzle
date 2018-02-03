module Main where

import qualified PalindromeNumber

main :: IO ()
main = do
  putStrLn "01. palindrome number"
  print $ PalindromeNumber.solve
