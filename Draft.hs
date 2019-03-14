module Draft where

fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci x _ 0 = x
fibonacci _ y 1 = y
fibonacci x y n = fibonacci y (x + y) (n - 1)
