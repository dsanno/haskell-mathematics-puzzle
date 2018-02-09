module MenWemen
( solve
) where


countUnbalanced' :: Int -> Int -> Int -> Int -> Int
countUnbalanced' n1 m1 n2 0
  | m1 >= n1 && m1 < n1 + n2 = 0
  | otherwise = 1
countUnbalanced' n1 m1 0 m2
  | n1 >= m1 && n1 < m1 + m2 = 0
  | otherwise = 1
countUnbalanced' 0 0 n2 m2 =
  countUnbalanced' 1 0 (n2 - 1) m2 +
  countUnbalanced' 0 1 n2 (m2 - 1)
countUnbalanced' n1 m1 n2 m2
  | n1 == m1 || n2 == m2 = 0
  | otherwise =
    countUnbalanced' (n1 + 1) m1 (n2 - 1) m2 +
    countUnbalanced' n1 (m1 + 1) n2 (m2 - 1)

countUnbalanced :: Int -> Int -> Int
countUnbalanced = countUnbalanced' 0 0

solve :: Int
solve = countUnbalanced 20 10
