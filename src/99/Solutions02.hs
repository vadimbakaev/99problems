module Solutions02 where
import Data.List

minimumAngle :: Int -> Int -> Int
minimumAngle h m
            | h >= 0 && h < 24 && m >= 0 && m < 60 = if angle > 180 then 360 - angle else angle
            | otherwise                            = error "Invalid arguments"
                                                     where angle           = abs $ fullHourAngle - fullMinuteAngle
                                                           fullHourAngle   = h `mod` 12 * gradePerHour
                                                           fullMinuteAngle = m * gradePerMinute
                                                           gradePerHour    = 360 `div` 12
                                                           gradePerMinute  = 360 `div` 60

data Encode a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [Encode a]
encodeModified xs = map toEncode $ group xs
                    where toEncode :: [a] -> Encode a
                          toEncode [a]         = Single a
                          toEncode as@(a:rest) = Multiple (length as) a

decodeModified :: Eq a => [Encode a] -> [a]
decodeModified = concatMap decode
                    where decode (Multiple n a) = replicate n a
                          decode (Single a)     = [a]

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
            | n < 1 = []
            | otherwise = take (n - 1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs n m = (take (m - n + 1) . drop (n - 1)) xs

rotate :: [a] -> Int -> [a]
rotate xs n =  drop k xs ++ take k xs
              where k = n `mod` length xs

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), [x | (x, i) <- xs `zip` [1..], i /= n])