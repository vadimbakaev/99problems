module Exercise where

import Data.List.NonEmpty

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x : xs) = x ++ flatten xs

flatten2 :: [[a]] -> NonEmpty a
flatten2 _ = undefined

flatten3 :: [NonEmpty a] -> [a]
flatten3 = flatten . (toList <$>)

flatten4 :: [NonEmpty a] -> NonEmpty a
flatten4 = undefined
  
flatten5 :: NonEmpty [a] -> [a]
flatten5 = flatten . toList
  
flatten6 :: NonEmpty [a] -> NonEmpty a
flatten6 = undefined 

flatten7 :: NonEmpty (NonEmpty a) -> [a]
flatten7 = flatten . (toList <$>) . toList
  
flatten8 :: NonEmpty (NonEmpty a) -> NonEmpty a
flatten8 ((x :| xs) :| xs') = x :| (xs ++ (flatten3 xs'))   
