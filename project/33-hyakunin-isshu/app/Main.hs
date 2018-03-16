module Main where

import           Control.Monad (forM_)
import qualified Data.List     as List

import           Lib

main :: IO ()
main = do
  Right rows <- loadCsv "data/q33.csv"
  let cols = List.transpose(List.tail rows)
      firsts = cols !! 3
      seconds = cols !! 4
      firstDecisionNumbers = decisionNumbers firsts
      secondDecisionNumbers = decisionNumbers seconds

  print $ sum firstDecisionNumbers + sum secondDecisionNumbers
  print "decisionNumbers for firsts:"
  forM_ (List.zipWith take firstDecisionNumbers firsts) print
  print "decisionNumbers for seconds:"
  forM_ (List.zipWith take secondDecisionNumbers seconds) print
  return ()
