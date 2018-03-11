module Lib
    ( countUnoverwrappedPaths
    ) where


import           Control.Monad       (mapM)
import           Control.Monad.State
import qualified Data.Map.Lazy       as Map

type Cache = Map.Map (Int, Int, Int) Int

withCache :: (Int, Int, Int) -> State Cache Int -> State Cache Int
withCache key f = do
  cache <- get
  case Map.lookup key cache of
    Just m -> return m
    _      -> do
                m <- f
                cache' <- get
                put $ Map.insert key m cache'
                return m

countPaths :: Int -> Int -> Int -> State Cache Int
countPaths w 1 0 = return 2
countPaths w 1 returnY = return 1
countPaths 1 h returnY
  | h == returnY = return returnY
  | otherwise = return $ returnY + 2
countPaths w h returnY = withCache (w, h, returnY) $
  case returnY of
    0 -> do
      rightNums <- mapM (countPaths (w - 1) h) [1..h]
      downNums <- mapM (countPaths (h - 1) w) [1..w]
      return $ sum rightNums + sum downNums
    _ -> do
      rightNums <- mapM (countPaths (w - 1) h) [returnY..h]
      downNum <- countPaths w (h - 1) (returnY - 1)
      return $ sum rightNums + downNum

countUnoverwrappedPaths :: Int -> Int -> Int
countUnoverwrappedPaths w h = evalState (countPaths w h 0) Map.empty
