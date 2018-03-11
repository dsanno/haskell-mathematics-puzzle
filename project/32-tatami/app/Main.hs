module Main where

import Lib

printAnswer :: Int -> Int -> IO ()
printAnswer w h =
  case arrange w h of
    Just x -> printMap w h x
    _ -> print "Not found"

main :: IO ()
main = do
  putStrLn "for 4 3"
  printAnswer 4 3
  putStrLn "for 4 7"
  printAnswer 4 7
  putStrLn "for 5 6"
  printAnswer 5 6
