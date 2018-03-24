module Lib
    ( searchWithCache
    , expandCups
    , makeCups
    , Status
    , Node(..)
    ) where

import           Control.Monad (mapM)
import           Control.Monad.State
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


type Status = (Int, Int, Int)
data Node a b = Inner [a] | Leaf b
  deriving Show


-- | searchWithCache
--
-- $setup
-- >>> e x = if x <= 0 then Leaf [] else Inner [0..(x-1)]
-- >>> comp x y = compare (length x) (length y)
-- >>> conv = flip (:)
--
-- >>> evalState (searchWithCache e comp conv [] 5) Map.empty
-- [5,4,3,2,1,0]

searchWithCache :: (Ord a) => (a -> Node a b) -> (b -> b -> Ordering) -> (b -> a -> b) -> b -> a -> State (Map a b) b
searchWithCache expand comp convert worst x =
  withCache x worst $
    case expand x of
      Leaf v -> return $ convert v x
      Inner nodes -> do
        ys <- mapM (searchWithCache expand comp convert worst) nodes
        return $ convert (List.maximumBy comp ys) x


-- | execute with cache
--
-- >>> runState (withCache 1 0 (do return 10)) Map.empty
-- (10,fromList [(1,10)])
-- >>> runState (withCache 1 0 (do return 10)) $ Map.fromList [(1,20)]
-- (20,fromList [(1,20)])

withCache :: (Ord k) => k -> v -> State (Map k v) v -> State (Map k v) v
withCache k defaultValue f = do
  cache <- get
  case Map.lookup k cache of
    Just v -> return v
    _ -> do
      modify $ Map.insert k defaultValue
      v <- f
      modify $ Map.insert k v
      return v

-- | expand cups statuses
--
-- >>> expandCups (8,5,3) (4,4,0)
-- Leaf True
-- >>> expandCups (8,5,3) (5,2,1)
-- Inner [(2,5,1),(7,0,1),(3,2,3),(6,2,0),(5,0,3),(5,3,0)]

expandCups :: Status -> Status -> Node Status Bool
expandCups (ma, mb, mc) (a, b, c)
  | a == div ma 2 = Leaf True
  | otherwise = Inner [(a1, b1, c), (a2, b2, c), (a3, b, c3), (a4, b, c4), (a, b5, c5), (a, b6, c6)]
  where
    d1 = min a (mb - b)
    a1 = a - d1
    b1 = b + d1
    d2 = min (ma - a) b
    a2 = a + d2
    b2 = b - d2
    d3 = min a (mc - c)
    a3 = a - d3
    c3 = c + d3
    d4 = min (ma - a) c
    a4 = a + d4
    c4 = c - d4
    d5 = min b (mc - c)
    b5 = b - d5
    c5 = c + d5
    d6 = min (mb - b) c
    b6 = b + d6
    c6 = c - d6


-- | make cup trios
--
-- >>> makeCups 2 6
-- [(4,3,1),(6,5,1)]
-- >>> makeCups 2 8
-- [(4,3,1),(6,5,1),(8,5,3),(8,7,1)]
-- >>> makeCups 6 10
-- [(6,5,1),(8,5,3),(8,7,1),(10,7,3),(10,9,1)]

makeCups :: Int -> Int -> [Status]
makeCups n m = List.concatMap makeCups' [n, (n + 2)..m]

makeCups' :: Int -> [Status]
makeCups' n = [(n, i, n - i) | i <- [(div n 2 + 1)..(n - 1)], gcd i (n - i) == 1]
