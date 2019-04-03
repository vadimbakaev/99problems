module Solutions03 where
import Data.List
import System.Random
import Control.Monad
import System.Random.Shuffle
import Data.Function
import qualified Data.Map as M

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

rndPermu :: [a] -> IO [a]
rndPermu xs = do gen <- newStdGen
                 return $ shuffle' xs (length xs) gen

combinations :: Eq a => Int -> [a] -> [[a]]
combinations n = nub . map (take n) . permutations

lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort xs = concatSortByLength $ concatSortByLength (M.elems (freqMap $ reverse $ M.elems (freqMap xs)))
    where concatSortByLength = concat . sortBy (compare `on` length)
          freqMap xs = M.fromListWith (++) (map (\x -> (length x, [x])) (reverse xs))
