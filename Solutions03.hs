module Solutions03 where
import Data.List
import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ x:right
    where (left, right) = splitAt (i - 1) xs

range :: Int -> Int -> [Int]
range m n | m == n     = [n]
          | m < n      = m : (range (m + 1) n)
          | otherwise  = m : (range (m - 1) n)

rnd_select :: [a] -> Int -> IO [Int]
rnd_select xs n = do r <- randomIO :: IO Int
                     return [r `mod` length xs]
