module Solutions04 where

isPrime :: Integer -> Bool
isPrime n = n > 1 && null [x | x <- [2..sqrtN n], n `mod` x == 0]
    where sqrtN = floor . sqrt . fromInteger

myGCD :: Int -> Int -> Int
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b) 

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

totient :: Int -> Int
totient n = length [x | x <- [1 .. n - 1], gcd x n == 1]
