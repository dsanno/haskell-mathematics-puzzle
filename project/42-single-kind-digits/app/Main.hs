module Main where

import qualified Data.Maybe as Maybe
import qualified Data.List  as List

import Lib

calc :: String -> Maybe Int
calc s = calculate . parse $ tokenize tokenizers s

makeRepeates :: Int -> String -> [String]
makeRepeates n = map (replicate n)

makeAllPatterns :: Int -> String -> String -> [String]
makeAllPatterns n symbols str = List.concatMap (makePatterns symbols) $ makeRepeates n str

main :: IO ()
main = print ys
  where
    symbols = "+-*/"
    digits = "123456789"
    target = 1234
    xs = map (\n -> makeAllPatterns n symbols digits) [1..]
    ys = List.find (not . null) $ map (List.filter (\x -> calc x == Just target)) xs
