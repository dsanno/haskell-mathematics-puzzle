module Lib
    ( countPaths
    ) where


import           Control.Monad.State
import qualified Data.Set            as Set

type Size = (Int, Int)
type Point = (Int, Int)
type Flag = Set.Set (Point, Point)

turnLeft :: Point -> Point
turnLeft (x, y) = (y, -x)

isIn :: Size -> Point -> Bool
isIn (w, h) (x, y) = x >= 0 && x < w && y >= 0 && y < h

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

move :: Size -> Point -> Point -> Maybe Point
move size pos m
  | isIn size newPos = Just newPos
  | otherwise = Nothing
  where newPos = add pos m

search :: Size -> Point -> Point -> Point -> State Flag Int
search size goal pos dir
  | goal == pos = return 1
  | otherwise = do
    cache <- get
    ns <- mapM (\dir ->
                 case move size pos dir of
                   Just p ->
                     if Set.member (pos, p) cache || Set.member (p, pos) cache
                     then return 0
                     else do
                       modify $ Set.insert (pos, p)
                       m <- search size goal p dir
                       modify $ Set.delete (pos, p)
                       return m
                   Nothing -> return 0
               ) [dir, turnLeft dir]
    return $ sum ns


countPaths :: Int -> Int -> Int
countPaths width height =
  evalState s Set.empty
  where s = search (width + 1, height + 1) (width, 0) (0, height) (1, 0)
