{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module C11_ADTs.Goats where

class TooMany a where
  tooMany :: a -> Bool 

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where 
  tooMany (n, _) = n > 1000

instance TooMany (Int, Int) where
  tooMany (n, m) = (n+m) > 84

instance (Num a, TooMany a, Ord a) => 
        TooMany (a, a) where
  tooMany (x, y) = (x+y) > 200
