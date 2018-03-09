module Main where

import Lib

clubs = [ (11000, 40)
        , (8000, 30)
        , (400, 24)
        , (800, 20)
        , (900, 14)
        , (1800, 16)
        , (1000, 15)
        , (7000, 40)
        , (100, 10)
        , (300, 12)
        ]

main :: IO ()
main = do
  let flags = distribute 150 clubs
      cs = map fst . filter snd $ zip clubs flags
      area = sum $ map fst cs
      member = sum $ map snd cs
  print flags
  print area
  print member
