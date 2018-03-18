module Lib
    ( findMinimumSwitches
    , countSwitches
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State
import           Data.Bits           as Bits
import           Data.Function       (on)
import qualified Data.List           as List
import qualified Data.Map.Lazy       as Map


type Cache = Map.Map (Int, [Int]) [Int]

findMinimumSwitches :: [Int] -> [Int]
findMinimumSwitches xs =
  evalState (findMinimumSwitches' xs) emptyCache

findMinimumSwitches' :: [Int] -> State Cache [Int]
findMinimumSwitches' xs = do
  ys <- mapM (\x -> findMinimumSwitchesWithHead x (List.delete x xs)) xs
  return $ List.minimumBy (compare `on` countSwitches) ys

findMinimumSwitchesWithHead :: Int -> [Int] -> State Cache [Int]
findMinimumSwitchesWithHead h [] = return [h]
findMinimumSwitchesWithHead h [x] = return [h, x]
findMinimumSwitchesWithHead h xs = do
  ys <- mapM (\x -> findMinimumSwitchesWithHead x (List.delete x xs)) xs
  return $ List.minimumBy (compare `on` countSwitches) (map (h:) ys)

emptyCache :: Cache
emptyCache = Map.empty

withCache :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
withCache key f = do
  cache <- get
  case Map.lookup key cache of
    Just value -> return value
    _ -> do
           value <- f
           modify $ Map.insert key value
           return value

countSwitches :: [Int] -> Int
countSwitches [] = 0
countSwitches (x:xs) = sum . map (m Map.!) $ zip (x:xs) xs
  where
    m = switchMap

switchMap :: Map.Map (Int, Int) Int
switchMap = Map.fromList patterns
  where
    pairs = (,) <$> segmentPatterns <*> segmentPatterns
    patterns = map (\((i, s), (i', s')) -> ((i, i'), countDigitSwitches s s')) pairs

countDigitSwitches :: [Int] -> [Int] -> Int
countDigitSwitches x y = List.foldl (\s (a, b) -> s + abs (a - b)) 0 $ zip x y

segmentPatterns :: [(Int, [Int])]
segmentPatterns =
  [ (0, [1, 1, 1, 1, 1, 1, 0])
  , (1, [0, 1, 1, 0, 0, 0, 0])
  , (2, [1, 1, 0, 1, 1, 0, 1])
  , (3, [1, 1, 1, 1, 0, 0, 1])
  , (4, [0, 1, 1, 0, 0, 1, 1])
  , (5, [1, 0, 1, 1, 0, 1, 1])
  , (6, [1, 0, 1, 1, 1, 1, 1])
  , (7, [1, 1, 1, 0, 0, 0, 0])
  , (8, [1, 1, 1, 1, 1, 1, 1])
  , (9, [1, 1, 1, 1, 0, 1, 1])
  ]
