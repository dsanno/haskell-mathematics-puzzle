module Main where

import qualified Data.Maybe as Maybe
import qualified Data.List  as List

import Lib


minimumZeroSevenNumber :: Int -> Maybe Int
minimumZeroSevenNumber n = List.find ((== 0) . (`mod` n)) zeroSevenNumbers

zeroSevenNumbers :: [Int]
zeroSevenNumbers = map ((7 *) . toBinary) [1..]

main :: IO ()
main = do
  let ns = [1..50]
      ms = map minimumZeroSevenNumber ns
  print $ filter (isPalindrome . intToList . snd) $ Maybe.catMaybes $ zipWith (\x y -> (,) x <$> y) ns ms
