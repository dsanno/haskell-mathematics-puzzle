module Main where

import           Data.Function (on)
import qualified Data.List     as List
import           Text.Printf   (printf)

import           Lib

diff :: (Fractional a) => a -> Registance a -> a
diff x p = abs (calculate p - x)

main :: IO ()
main = do
  let v = 1.0 :: Double
      patterns = makePatterns v 10
      target = (sqrt 5 + 1) / 2 :: Double
      bestPattern = List.minimumBy (on compare (diff target)) patterns
  print bestPattern
  printf "%.10f\n" $ calculate bestPattern
