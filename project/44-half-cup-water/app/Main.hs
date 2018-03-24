module Main where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map

import           Lib


try :: (Int, Int, Int) -> Bool
try (a, b, c) = evalState (search (a, 0, 0)) Map.empty
  where
    expand = expandCups (a, b, c)
    search = searchWithCache expand compare const False

main :: IO ()
main = do
  let statuses = makeCups 10 100
      success = filter try statuses
  print $ length statuses
  print $ length success
