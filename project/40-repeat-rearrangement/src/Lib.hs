module Lib
    ( Node(..)
    , tryRearrange
    , search
    ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Node a b = Inner [a] | Leaf b

tryRearrange :: Int -> [Maybe Int] -> Maybe [Maybe Int]
tryRearrange n xs =
  case List.elemIndex (Just n) xs of
    Just m
      | m == n - 1 -> Just $ reverse xs' ++ remain
      | otherwise -> Nothing
    Nothing
      | Maybe.isNothing (xs List.!! (n - 1)) -> Just $ (Just n:(tail . reverse) xs') ++ remain
      | otherwise -> Nothing
  where
    (xs', remain) = List.splitAt n xs

search :: (a -> Node a b) -> (b -> b -> Ordering) -> (a -> b -> b) -> a -> b
search expand compareLeaf postProcess a =
  case expand a of
    Inner xs -> postProcess a b
      where
        ys = map (search expand compareLeaf postProcess) xs
        b = List.maximumBy compareLeaf ys
    Leaf x -> x
