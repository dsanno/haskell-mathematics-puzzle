module Main where

import qualified PalindromeNumber
import qualified Calculator
import qualified Flip
import qualified CutStick
import qualified MoneyExchange
import qualified Collatz
import qualified BinaryDate
import qualified RobotCleaner
import qualified MenWemen
import qualified Roulette
import qualified Fibonacci
import qualified SquareRoot
import qualified VerbalArithmetic

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
  putStrLn "06. collatz"
  print Collatz.solve
  putStrLn "07. binary date"
  print BinaryDate.solve
  putStrLn "08. robot cleaner"
  print RobotCleaner.solve
  putStrLn "09. unbalance men wemen"
  print MenWemen.solve
  putStrLn "10. roulette max value"
  print Roulette.solve
  putStrLn "11. fibonacci"
  print Fibonacci.solve
  putStrLn "12. sqaure root"
  print SquareRoot.solve
  putStrLn "13. verbal arithmetic"
  print VerbalArithmetic.solve
