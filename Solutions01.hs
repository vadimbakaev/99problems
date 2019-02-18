module Solutions01 where
import Criterion.Main
import Data.List

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength = foldl (const . (+1)) 0

myReverse :: [a] -> [a]
myReverse xs = reverseInto [] xs
               where reverseInto acc []     = acc
                     reverseInto acc (x:xs) = reverseInto (x : acc) xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress (e:xs) = e : compress (dropWhile (==e) xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (e:xs) = [[e] ++ takeWhile (==e) xs] ++ pack (dropWhile (==e) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

-- main = defaultMain [
--   bgroup "myLast" [ bench "standart"  $ whnf myLast [1..1000]
--                   , bench "prime"  $ whnf myLast' [1..1000]
--                   ]
--   ]