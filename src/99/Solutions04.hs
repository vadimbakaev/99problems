module Solutions04 where

import Data.List
import Data.Maybe

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

primeFactors :: Integer -> [Integer]
primeFactors n | isPrime n = [n]
               | otherwise = prime : primeFactors (n `div` prime)
    where prime = head [p | p <- primes, gcd n p /= 1]
          primes = filterPrime [2..]
              where filterPrime (p:xs) = p:[x | x <- xs, x `mod` p /= 0]

primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult = map (\xs -> (head xs, length xs)) . group . primeFactors

primeR :: Integer -> Integer -> [Integer]
primeR l r = (takeWhile (<r) . dropWhile (<l)) primes
    where primes = map head (iterate crossout [2..])
          crossout (x:xs) = filter ((0 /=) . (`mod` x)) xs
               
goldbatch :: Int -> (Int, Int)
goldbatch n = head [(a, b) | (a, b) <- (,) <$> primesN <*> primesN, a + b == n]
    where primesN = take n primes
          primes = map head (iterate crossout [2..])
          crossout (x:xs) = [x' | x' <- xs, x' `mod` x /= 0]

goldbatch' :: Int -> Maybe (Int, Int)
goldbatch' n = listToMaybe $ take 1 [(a, b) | (a, b) <- (,) <$> primesN <*> primesN, a + b == n]
    where primesN = take n primes
          primes = map head (iterate crossout [2..])
          crossout (x:xs) = [x' | x' <- xs, x' `mod` x /= 0]
          
                           
goldbatchList :: Int -> Int -> [(Int, Int)]
goldbatchList l r = catMaybes (map goldbatch' ([l',l'+2..r] \\ primesN))
    where primesN = takeWhile (<r) primes
          primes = map head (iterate crossout [2..])
          l' = if l `mod` 2 == 0 then l else l - 1
          crossout (x:xs) = filter ((/=0).(`mod` x)) xs
