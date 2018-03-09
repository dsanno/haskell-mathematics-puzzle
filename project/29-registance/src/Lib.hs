module Lib
    ( Registance(..)
    , calculate
    , makePatterns
    ) where

import qualified Data.List as List

data Registance a = Node a
                  | Series (Registance a) (Registance a)
                  | Parallel (Registance a) (Registance a)
                  deriving (Show, Eq)

calculate :: (Fractional a) => Registance a -> a
calculate (Node a) = a
calculate (Series a b) = calculate a + calculate b
calculate (Parallel a b) = 1 / (c + d)
  where
    c = 1 / calculate a
    d = 1 / calculate b

makePatterns :: (Fractional a) => a -> Int -> [Registance a]
makePatterns v 1 = [Node v]
makePatterns v n = List.concatMap (\i -> seriesPatterns v i (n - i) ++ parallelPatterns v i (n - i)) [1..(n - 1)]
  where
    seriesPatterns v i j = Series <$> makePatterns v i <*> makePatterns v j
    parallelPatterns v i j = Parallel <$> makePatterns v i <*> makePatterns v j
