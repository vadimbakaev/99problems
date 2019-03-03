module Solutions03 where
import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = left ++ x:right
    where (left, right) = splitAt (i - 1) xs