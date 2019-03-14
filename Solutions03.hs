module Solutions03 where
import Data.List
import System.Random
import Control.Monad

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ x:right
    where (left, right) = splitAt (i - 1) xs

range :: Int -> Int -> [Int]
range m n | m == n     = [n]
          | m < n      = m : range (m + 1) n
          | otherwise  = m : range (m - 1) n

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do ys <- replicateM n (getStdRandom(randomR(0, length xs - 1)))
                     return $ map (xs !!) ys

diffSelect :: Int -> Int -> IO [Int]
diffSelect x n = replicateM x (getStdRandom (randomR(1, n)))
