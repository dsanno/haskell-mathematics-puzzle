module Calculator
( solve
) where

import           Data.Maybe (mapMaybe)
import           Data.Ratio (numerator, denominator)
import qualified Number     as N


data Token =   Digit Int
             | Number Rational
             | Add
             | Subtract
             | Multiply
             | Divide

calculate :: [Token] -> Maybe Rational
calculate [Number x] = Just x
calculate (Number x : Add : rest) =
  case calculateTerm rest of
    Just (_, Number y : rest') -> calculate $ (Number $ x + y) : rest'
    _                          -> Nothing
calculate (Number x : Subtract : rest) =
  case calculateTerm rest of
    Just (_, Number y : rest') -> calculate $ (Number $ x - y) : rest'
    _                          -> Nothing
calculate xs =
  case calculateTerm xs of
    Just (True, ys) -> calculate ys
    _               -> Nothing

calculateTerm :: [Token] -> Maybe (Bool, [Token])
calculateTerm (Number x : Multiply : Number y : rest) = do
  (_, r) <- calculateTerm ((Number $ x * y) : rest)
  return (True, r)
calculateTerm (Number x : Divide : Number 0 : rest) = Nothing
calculateTerm (Number x : Divide : Number y : rest) = do
  (_, r) <- calculateTerm ((Number $ x / y) : rest)
  return (True, r)
calculateTerm xs = Just (False, xs)

shrinkDigits :: [Token] -> [Token]
shrinkDigits []               = []
shrinkDigits (Digit n : rest) = shrinkDigits' (toRational n) rest
shrinkDigits (x:xs)           = x:shrinkDigits xs

shrinkDigits' :: Rational -> [Token] -> [Token]
shrinkDigits' x (Digit n : rest) = shrinkDigits' (x * 10 + toRational n) rest
shrinkDigits' x xs               = Number x : shrinkDigits xs

generateCandidates :: [Int] -> [[Token]]
generateCandidates [] = []
generateCandidates [x] = [[Digit x]]
generateCandidates (x:xs) =
  let ys = generateCandidates xs
      prependers = map ((Digit x :) .) [(Add :), (Subtract :), (Multiply :), (Divide :), id]
  in concatMap (`map` ys) prependers

solveNumber :: Int -> Bool
solveNumber x =
  let ds = N.digits 10 x
      ys = mapMaybe (calculate . shrinkDigits) $ filter (\cs -> length cs > length ds) $ generateCandidates ds
      zs = mapMaybe (\x -> case denominator x of
                      1 -> Just $ N.reverse $ fromIntegral $ numerator x :: Maybe Int
                      _ -> Nothing
                    ) ys
  in elem x zs

solve :: [Int]
solve = filter solveNumber [1000..9999]
