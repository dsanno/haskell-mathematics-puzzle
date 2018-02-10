module Roulette
( solve
) where


european :: [Int]
european = [ 0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36
           , 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9
           , 22, 18, 29, 7, 28, 12, 35, 3, 26]

american :: [Int]
american = [ 0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15
           , 3, 24, 36, 13, 1, 0, 27, 10, 25, 29, 12, 8, 19, 31
           , 18, 6, 21, 33, 16, 4, 23, 35, 14, 2]

maxSum :: Int -> [Int] -> Int
maxSum n xs =
  let (as, bs) = splitAt n xs
      sums = scanl1 (+) $ zipWith (-) (bs ++ as) xs
  in sum as + maximum sums

maxSums :: [Int] -> [Int] -> [Int]
maxSums ns xs = map (`maxSum` xs) ns

solve :: Int
solve =
  let ns = [2..36]
      es = maxSums ns european
      as = maxSums ns american
  in sum . map fromEnum $ zipWith (<) es as
