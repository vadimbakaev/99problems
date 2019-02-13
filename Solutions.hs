module Solutions where

myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs

myButLast :: [a] -> a
myButLast (x : _ : []) = x
myButLast (x : xs)     = myButLast xs

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