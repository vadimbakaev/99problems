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
                          toEncode (a:[])          = Single a
                          toEncode as@(a:rest)     = Multiple (length as) a