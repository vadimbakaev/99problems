module Solutions03 where
import Data.List
import System.Random
import System.Random.Shuffle

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ x:right
    where (left, right) = splitAt (i - 1) xs

range :: Int -> Int -> [Int]
range m n | m == n     = [n]
          | m < n      = m : (range (m + 1) n)
          | otherwise  = m : (range (m - 1) n)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do gen <- newStdGen
                     return $ take n (shuffle' xs (length xs) gen)

