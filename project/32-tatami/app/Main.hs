module Main where

import Control.Monad (forM_)

import Lib

printAnswer :: Int -> Int -> IO ()
printAnswer w h =
  forM_ (arrange w h) (\m -> do
    printMap w h m
    putStrLn ""
  )

main :: IO ()
main = do
  putStrLn "for 4 3"
  printAnswer 4 3
  putStrLn "for 7 4"
  printAnswer 7 4
  putStrLn "for 6 5"
  printAnswer 6 5
