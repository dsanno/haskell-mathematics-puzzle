module Main where

import qualified PalindromeNumber
import qualified Calculator
import qualified Flip
import qualified CutStick
import qualified MoneyExchange

main :: IO ()
main = do
  putStrLn "01. palindrome number"
  print PalindromeNumber.solve
  putStrLn "02. calculator"
  print Calculator.solve
  putStrLn "03. flip cards"
  print Flip.solve
  putStrLn "04. cut sticks"
  print CutStick.solve
  putStrLn "05. money exchange"
  print MoneyExchange.solve
