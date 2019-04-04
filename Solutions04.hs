module Solutions04 where

isPrime :: Integer -> Bool
isPrime n = n > 1 && null [x | x <- [2..sqrtN n], n `mod` x == 0]
    where sqrtN = floor . sqrt . fromInteger
