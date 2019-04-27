module Solution05 where

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = do
  let tf = [True, False]
  a <- tf
  b <- tf
  return (a, b, f a b)
