module Chap16.EitherFunc where

import Test.QuickCheck

data Sum' a b =
    First' a 
  | Second b
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Sum' a b) where
  arbitrary = sumGen 

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum' a b) 
sumGen = do
    a <- arbitrary 
    b <- arbitrary 
    frequency [(1, return $ First' a), (3, return $ Second b)]

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool 
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x) 

sumTest :: IO () 
sumTest = do
   quickCheck (functorIdentity :: Sum' String Double -> Bool) 
   let c = functorCompose (+1) (*2)
   let fc x = c (x :: Sum' String Int)
   quickCheck fc
