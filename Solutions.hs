module Solutions where
import Criterion.Main

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength = loop 0
           where loop n []     = n
                 loop n (x:xs) = loop (n + 1) xs

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

-- main = defaultMain [
--   bgroup "myLast" [ bench "standart"  $ whnf myLast [1..1000]
--                   , bench "prime"  $ whnf myLast' [1..1000]
--                   ]
--   ]