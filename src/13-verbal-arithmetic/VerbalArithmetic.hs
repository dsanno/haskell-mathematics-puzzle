module VerbalArithmetic
( solve
) where


import           Data.List       as List
import           Data.Map.Strict as Map
import           Data.Maybe      as Maybe


toNumber :: Map.Map Char Int -> String -> Maybe Int
toNumber m (c:_)
  | Map.lookup c m == Just 0 = Nothing
toNumber m s = List.foldl (\total c -> (\t n -> t * 10 + n) <$> total <*> Map.lookup c m) (Just 0) s

(<+>) :: Maybe Int -> Maybe Int -> Maybe Int
(<+>) x y = (+) <$> x <*> y

confirm :: Map.Map Char Int -> Bool
confirm m =
  let number = toNumber m
      x = number "READ" <+> number "WRITE" <+> number "TALK"
      y = number "SKILL"
  in x == y && Maybe.isJust y

solveArithmetic :: String -> [Int] -> Map.Map Char Int -> [Map.Map Char Int]
solveArithmetic [] _ m
  | confirm m = [m]
  | otherwise = []
solveArithmetic (k:ks) vs m =
  List.concatMap (\v -> solveArithmetic ks (List.delete v vs) (Map.insert k v m)) vs


solve :: Int
solve =
  let keys = nub "READWRITETALKSKILL"
  in List.length . List.map Map.toList $ solveArithmetic keys [0..9] Map.empty
