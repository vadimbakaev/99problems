module Draft where

fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci x _ 0 = x
fibonacci _ y 1 = y
fibonacci x y n = fibonacci y (x + y) (n - 1)

solve :: [Int] -> Int
solve [db, mb, yb, de, me, ye] | yb > ye = 1000
                               | mb > me = 500 * (mb - me)
                               | otherwise = 15 * maximum [db - de, 0]
