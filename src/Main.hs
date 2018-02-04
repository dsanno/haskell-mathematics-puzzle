module Main where

import qualified PalindromeNumber
import qualified Calculator
import qualified Flip

main :: IO ()
main = do
  putStrLn "01. palindrome number"
  print PalindromeNumber.solve
  putStrLn "02. calculator"
  print Calculator.solve
  putStrLn "03. flip cards"
  print Flip.solve
