module Main where

import           Data.Function (on)
import qualified Data.Maybe    as Maybe

import Lib

type Sequence = [Maybe Int]

expand :: Sequence -> Node Sequence [Sequence]
expand x =
  case ys of
    [] ->  Leaf [x]
    _ -> Inner ys
  where ys = Maybe.mapMaybe (`tryRearrange` x) [2..(length x)]

main :: IO ()
main = do
  let maxPath = search expand (compare `on` length) (:) $ Just 1 : replicate 8 Nothing
  print maxPath
  print $ length maxPath - 1
