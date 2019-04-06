module C17_Applicative.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data List' a = 
    Nil' 
  | Cons' a (List' a)
  deriving (Eq, Show)

instance Functor List' where
  fmap _ Nil' = Nil' 
  fmap f (Cons' x xs) = Cons' (f x) (fmap f xs)

-- (<*>) with direct pattern matching
instance Applicative List' where
  pure x = Cons' x Nil'

  Nil' <*> _ = Nil' 
  _ <*> Nil' = Nil'
  fs <*> xs = go fs xs Nil'
    where
      go Nil' _ acc = acc 
      go (Cons' f fs) Nil' acc = go fs xs acc
      go (Cons' f fs) (Cons' x xs) acc = go (Cons' f fs) xs (appendVal (f x) acc)

appendVal :: a -> List' a -> List' a 
appendVal x Nil' = Cons' x Nil'
appendVal x (Cons' y ys) = Cons' y (appendVal x ys)

-- Testing
instance Eq a =>
        EqProp (List' a) where
  (=-=) = eq

instance Arbitrary a =>
        Arbitrary (List' a) where
  arbitrary = lisGen 

lisGen :: Arbitrary a => Gen (List' a)
lisGen = do
    a <- arbitrary 
    b <- arbitrary
    frequency [(1, return Nil'), (3, return $ Cons' a b)]

type SSI = (String, String, Int)

trigger :: List' SSI
trigger = undefined

checkerList :: IO () 
checkerList = quickBatch (applicative trigger)
