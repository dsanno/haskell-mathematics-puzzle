module Lib
    ( toggle
    , listToBits
    , printBits
    , findMaxMove
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State
import           Data.Bits           (xor, (.&.))
import qualified Data.Bits           as Bits
import qualified Data.List           as List
import qualified Data.Map.Lazy       as Map


type Point = (Int, Int)
type Cache = Map.Map Int Int

size :: Int
size = 4

allPoints :: [Point]
allPoints = [(i, j) | i <- [0..(size - 1)], j <- [0..(size - 1)]]

printBits :: Int -> IO ()
printBits bits = do
  let xs = bitsToList bits
  foldM_ (\xs' _ -> do
           let (row, remain) = List.splitAt size xs'
           print row
           return remain
         ) xs [1..size]

findMaxMove :: (Int, Int)
findMaxMove = evalState (findMaxMoveWithCache 0 0) $ Map.singleton 0 0

findMaxMoveWithCache :: Int -> Int -> State Cache (Int, Int)
findMaxMoveWithCache bits n = do
  cache <- get
  let xs = List.filter ((== n) . snd) $ Map.toList cache
      ys = List.concatMap (\(b, depth) -> map (\p -> (toggle b p, depth + 1)) allPoints) xs :: [(Int, Int)]
  if all (\(b, _) -> Map.member b cache) ys
  then return $ List.head xs
  else do
    put $ Map.union cache $ Map.fromList ys
    findMaxMoveWithCache bits (n + 1)

toggle :: Int -> Point -> Int
toggle bits (x, y) = List.foldl togglePoint bits $ ps ++ qs
  where
    ps = [(i, y) | i <- [0..(size - 1)]]
    qs = [(x, i) | i <- [0..(size - 1)], i /= y]

togglePoint :: Int -> Point -> Int
togglePoint bits (x, y) = Bits.complementBit bits (x + y * size)

listToBits :: [Int] -> Int
listToBits = List.foldl Bits.setBit 0

bitsToList :: Int -> [Int]
bitsToList x = map (fromEnum . Bits.testBit x) [0..(size * size - 1)]
