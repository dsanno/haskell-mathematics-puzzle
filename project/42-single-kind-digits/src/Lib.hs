module Lib
    ( makePatterns
    , tokenize
    , parse
    , calculate
    , tokenizers
    ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Token = Value Int | Operator (Int -> Int -> Maybe Int)
type Tokenizer = String -> Maybe ((Token, Int), String)

instance Show Token where
  show (Value x) = "Value " ++ show x
  show (Operator f) = "Operator"


-- | make string patterns
--
-- >>> makePatterns "+-" "a"
-- ["a"]
-- >>> makePatterns "+-" "ab"
-- ["ab","a+b","a-b"]
-- >>> makePatterns "+-" "abc"
-- ["abc","ab+c","ab-c","a+bc","a+b+c","a+b-c","a-bc","a-b+c","a-b-c"]

makePatterns :: String -> String -> [String]
makePatterns symbols [] = []
makePatterns symbols str@[c] = [str]
makePatterns symbols str@(c:cs) = map (c:) $ ps ++ ((:) <$> symbols <*> ps)
  where
    ps = makePatterns symbols cs

-- | tokenize
--
-- >>> tokenize [tokenizeDigits] "123"
-- [(Value 123,3)]
-- >>> tokenize [tokenizeDigits] "+123"
-- []
-- >>> tokenize [tokenizeDigits] "1+2"
-- []
-- >>> tokenize [tokenizeDigits, tokenizePlus] "12+23"
-- [(Value 12,3),(Operator,1),(Value 23,3)]
-- >>> tokenize [tokenizeDigits, tokenizePlus, tokenizeSlash] "12/2/3"
-- [(Value 12,3),(Operator,2),(Value 2,3),(Operator,2),(Value 3,3)]

tokenize :: [Tokenizer] -> String -> [(Token, Int)]
tokenize tokenizers s =
  case Maybe.mapMaybe (\f -> f s) tokenizers of
    (t, []):_ -> [t]
    (t, remain):_ ->
      case tokenize tokenizers remain of
        [] -> []
        ts -> t:ts
    _ -> []


-- | convert Strint to Int
--
-- >>> strToInt "0"
-- 0
-- >>> strToInt "123"
-- 123
strToInt :: String -> Int
strToInt = List.foldl (\n c -> n * 10 + Char.digitToInt c) 0

tokenizers :: [Tokenizer]
tokenizers =
  [ tokenizeDigits
  , tokenizePlus
  , tokenizeMinus
  , tokenizeAster
  , tokenizeSlash
  ]

tokenizeDigits :: Tokenizer
tokenizeDigits s =
  case xs of
    [] -> Nothing
    _ -> Just ((Value (strToInt xs), 3), ys)
  where
    (xs, ys) = List.span Char.isDigit s

tokenizeChar :: Char -> Token -> Int -> Tokenizer
tokenizeChar c op priority (x:xs)
  | x == c = Just ((op, priority), xs)
  | otherwise = Nothing


tokenizePlus :: Tokenizer
tokenizePlus = tokenizeChar '+' addOperator 1

tokenizeMinus :: Tokenizer
tokenizeMinus = tokenizeChar '-' subOperator 1

tokenizeAster :: Tokenizer
tokenizeAster = tokenizeChar '*' mulOperator 2

tokenizeSlash :: Tokenizer
tokenizeSlash = tokenizeChar '/' divOperator 2


addOperator :: Token
addOperator = Operator (\x y -> Just (x + y))

subOperator :: Token
subOperator = Operator (\x y -> Just (x - y))

mulOperator :: Token
mulOperator = Operator (\x y -> Just (x * y))

divOperator :: Token
divOperator = Operator safeDiv

-- | safe version of div
--
-- >>> safeDiv 4 2
-- Just 2
-- >>> safeDiv 4 0
-- Nothing
-- >>> safeDiv 4 3
-- Just 1
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y


-- | parse and convert to reverse Polish notation
--
-- >>> parse []
-- []
-- >>> parse [(Value 1, 3)]
-- [Value 1]
-- >>> parse [(Value 1, 3), (addOperator, 2), (Value 2, 3)]
-- [Value 1,Value 2,Operator]
-- >>> parse [(Value 1, 3), (addOperator, 1), (Value 2, 3), (subOperator, 1), (Value 3, 3)]
-- [Value 1,Value 2,Operator,Value 3,Operator]
-- >>> parse [(Value 1, 3), (addOperator, 1), (Value 2, 3), (mulOperator, 2), (Value 3, 3)]
-- [Value 1,Value 2,Value 3,Operator,Operator]
-- >>> parse [(Value 1, 3), (divOperator, 2), (Value 2, 3), (subOperator, 1), (Value 3, 3)]
-- [Value 1,Value 2,Operator,Value 3,Operator]
-- >>> parse [(Value 12, 3), (divOperator, 2), (Value 2, 3), (divOperator, 2), (Value 3, 3)]
-- [Value 12,Value 2,Operator,Value 3,Operator]
--
-- >>> parse [(addOperator, 1)]
-- []

parse :: [(Token, Int)] -> [Token]
parse = parse' []

parse' :: [(Token, Int)] -> [(Token, Int)] -> [Token]
parse' hs ts =
  case parseOperator hs ts of
    ([], _, _) -> []
    (tokens, [], []) -> tokens
    (tokens, histories, remain) -> tokens ++ parse' histories remain

parseOperator :: [(Token, Int)] -> [(Token, Int)] -> ([Token], [(Token, Int)], [(Token, Int)])
parseOperator [] [] = ([], [], [])
parseOperator hs [(Value n, _)] = (Value n : map fst hs, [], [])
parseOperator [] ((Value n, _):t@(Operator o', p'):ts) = ([Value n], [t], ts)
parseOperator (h@(Operator o, p):hs) ((Value n, _):t@(Operator o', p'):ts)
  | p' > p = ([Value n], t:h:hs, ts)
  | otherwise = ([Value n, Operator o], t:hs, ts)
parseOperator _ _ = ([], [], [])

-- | calculate tokens
-- >>> calculate [(Value 123)]
-- Just 123
-- >>> calculate [(Value 1), (Value 2), addOperator]
-- Just 3
-- >>> calculate [(Value 2), (Value 1), subOperator]
-- Just 1
-- >>> calculate [(Value 1), (Value 2), mulOperator, (Value 3), addOperator]
-- Just 5
-- >>> calculate [(Value 1), (Value 2), (Value 3), mulOperator, addOperator]
-- Just 7
-- >>> calculate [(Value 12), (Value 2), divOperator, (Value 3), divOperator]
-- Just 2

calculate :: [Token] -> Maybe Int
calculate = calculate' []

calculate' :: [Token] -> [Token] -> Maybe Int
calculate' [Value n] [] = return n
calculate' hs [] = Nothing
calculate' hs ts = stepCalculation hs ts >>= uncurry calculate'

stepCalculation :: [Token] -> [Token] -> Maybe ([Token], [Token])
stepCalculation hs (Value n : ts) = return (Value n : hs, ts)
stepCalculation (Value n2 : Value n1 : hs) (Operator o : ts) = do
  n <- o n1 n2
  return (Value n : hs, ts)
stepCalculation _ _ = Nothing
