module MoneyExchange
( solve
, test
) where


subExchange :: Int -> Int -> [(Int, Int)]
subExchange n total =
  let m = div total n
  in map (\x -> (x, total - n * x)) [0..m]

exchange :: [Int] -> Int -> Int -> [[Int]]
exchange ns maxCount total =
  let f x (ys, t) = map (\(y, t') -> (y:ys, t')) $ subExchange x t
      as = foldl (\acc n -> concatMap (f n) acc) [([], total)] ns
      in map (reverse . fst) $ filter (\(cs, t) -> sum cs <= maxCount && t == 0) as

solve :: Int
solve = length $ exchange [500, 100, 50, 10] 15 1000

test :: IO ()
test = print $ exchange [500, 100, 50, 10] 15 1000
