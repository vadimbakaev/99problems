module Draft where

import Control.Monad.Trans.Writer.Lazy
import Data.Monoid


fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci x _ 0 = x
fibonacci _ y 1 = y
fibonacci x y n = fibonacci y (x + y) (n - 1)

solve :: [Int] -> Int
solve [db, mb, yb, de, me, ye] | yb > ye = 1000
                               | mb > me = 500 * (mb - me)
                               | otherwise = 15 * maximum [db - de, 0]

toMaybe :: (Eq a) => a -> Maybe a
toMaybe = wrap
    where wrap :: a -> Maybe a
          wrap = Just
               
type Shopping = Writer [String] (Sum Integer)

purchase :: String -> Integer -> Shopping
purchase item cost = do
  tell [item]
  return $ Sum cost

total :: Shopping -> Integer
total = getSum . fst . runWriter

products :: Shopping -> [String]
products = execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
