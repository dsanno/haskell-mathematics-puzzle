module Lib
    ( arrange
    , printMap
    ) where


import qualified Data.Map.Lazy       as Map

type Point = (Int, Int)
type IdMap = Map.Map Point Int

nextPoint :: Int -> Int -> Point -> Point
nextPoint w h (x, y)
  | x + 1 < w = (x + 1, y)
  | otherwise = (0, y + 1)

isCross :: Point -> IdMap -> Bool
isCross (x, y) idMap = id1 /= id2 && id1 /= id3 && id2 /= id4 && id3 /= id4
  where
    id1 = Map.lookup (x, y) idMap
    id2 = Map.lookup (x + 1, y) idMap
    id3 = Map.lookup (x, y + 1) idMap
    id4 = Map.lookup (x + 1, y + 1) idMap

place :: Int -> Int -> Point -> Point -> IdMap -> [IdMap]
place w h p@(x, y) (dx, dy) idMap
  | x + dx >= w || y + dy >= h = []
  | Map.member (x + dx, y + dy) idMap = []
  | isCross (x + dx, y - 1) newMap = []
  | otherwise = [newMap]
  where
    newId = Map.size idMap `div` 2
    newMap = Map.insert (x, y) newId $ Map.insert (x + dx, y + dy) newId idMap

arrange' :: Int -> Int -> Point -> IdMap -> [IdMap]
arrange' w h p@(x, y) idMap
  | Map.size idMap == w * h = return idMap
  | Map.member p idMap = arrange' w h (nextPoint w h p) idMap
  | otherwise = m1 ++ m2
      where
        m1 = place w h p (1, 0) idMap >>= arrange' w h (nextPoint w h (x + 1, y))
        m2 = place w h p (0, 1) idMap >>= arrange' w h (nextPoint w h (x, y))

arrange :: Int -> Int -> [IdMap]
arrange w h = arrange' w h (0, 0) Map.empty

isHorizontal :: Point -> IdMap -> Maybe Bool
isHorizontal p@(x, y) m =
  case Map.lookup p m of
    Just i -> Just $ Map.lookup (x - 1, y) m == Just i || Map.lookup (x + 1, y) m == Just i
    _ -> Nothing

printLines :: Int -> String -> IO ()
printLines n s
  | null s = return ()
  | otherwise = do
      let (x, y) = splitAt n s
      putStrLn x
      printLines n y

printMap :: Int -> Int -> Map.Map (Int, Int) Int -> IO ()
printMap w h idMap = do
  let xs = [0..(w - 1)]
      ys = [0..(h - 1)]
  let points = flip (,) <$> [0..(h - 1)] <*> [0..(w - 1)]
      chars = map (\p -> case isHorizontal p idMap of
                           Just True -> '-'
                           Just False -> '|'
                           _ -> '.'
                  ) points
  printLines w chars
