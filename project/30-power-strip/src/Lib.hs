module Lib
    ( orderdCombinations
    , countPatterns
    ) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map.Lazy       as Map


type Cache = Map.Map Int Int

orderdCombinations :: Int -> Int -> Int -> [[Int]]
orderdCombinations 1 minNum total
  | total >= minNum && total >= 1 = [[total]]
  | otherwise = []
orderdCombinations size minNum total =
  List.concatMap (\n -> [(n:)] <*> orderdCombinations (size - 1) n (total - n)) [minNum..(div total size)]

countCombinations2 :: (Int -> State Cache Int) -> [Int] -> State Cache Int
countCombinations2 f [a, b]
  | a == b = do
      fa <- f a
      return $ fa * (fa + 1) `div` 2
  | otherwise = do
      fa <- f a
      fb <- f b
      return $ fa * fb

countCombinations3 :: (Int -> State Cache Int) -> [Int] -> State Cache Int
countCombinations3 f [a, b, c]
  | a == b && b == c = do
      fa <- f a
      return $ fa * (fa + 1) * (fa + 2) `div` 6
  | a == b = do
      fa <- f a
      fc <- f c
      return $ fa * (fa + 1) * fc `div` 2
  | b == c = do
      fa <- f a
      fb <- f b
      return $ fa * fb * (fb + 1) `div` 2
  | a == c = do
      fa <- f a
      fb <- f b
      return $ fa * (fa + 1) * fb `div` 2
  | otherwise = do
      fa <- f a
      fb <- f b
      fc <- f c
      return $ fa * fb * fc

withCache :: Int -> State Cache Int -> State Cache Int
withCache n f = do
  cache <- get
  case Map.lookup n cache of
    Just m -> return m
    _      -> do
                m <- f
                cache' <- get
                put $ Map.insert n m cache'
                return m

countPatternsWithCache :: Int -> State Cache Int
countPatternsWithCache n
  | n <= 0 = return 0
  | n == 1 = return 1
  | n == 2 = return 1
  | otherwise = withCache n $ do
      let c2 = orderdCombinations 2 1 n
          c3 = orderdCombinations 3 1 n
      pattern2s <- mapM (countCombinations2 countPatternsWithCache) c2
      patterb3s <- mapM (countCombinations3 countPatternsWithCache) c3
      return $ sum pattern2s + sum patterb3s

countPatterns :: Int -> Int
countPatterns n = evalState (countPatternsWithCache n) Map.empty
