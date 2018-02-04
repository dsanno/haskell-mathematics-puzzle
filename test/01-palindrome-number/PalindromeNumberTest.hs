module PalindromeNumberTest where

import Test.Tasty
import Test.Tasty.HUnit

tests = TestList
  [ "first" ~: 1 + 1 ~?= 2
  , "second" ~: 1 + 2 ~?= 2
  ]

run :: IO ()
run =
  runTestTT $ TestLabel "add" tests
--  runTestText (putTextToHandle stderr False) tests
