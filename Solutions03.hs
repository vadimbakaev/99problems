module Solutions03 where
import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ x:right
    where (left, right) = splitAt (i - 1) xs

range :: Int -> Int -> [Int]
range m n | m > n     = []
          | otherwise = m : (range (m + 1) n)