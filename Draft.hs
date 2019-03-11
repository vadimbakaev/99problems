module Draft where

fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci _ _ 0 = 0
fibonacci _ _ 1 = 1
fibonacci _ _ 2 = 1
fibonacci x y n | n == 3    = x + y
                | otherwise = fibonacci y (y+x) (n-1) 
