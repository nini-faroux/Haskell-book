module Chap16.Possibly where

import Test.QuickCheck

data Possibly a = 
    LolNope
  | Yeppers a 
  deriving (Eq, Show)

instance Functor Possibly where 
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a =>
        Arbitrary (Possibly a) where
  arbitrary = possGen 

possGen :: Arbitrary a => Gen (Possibly a)
possGen = do
    a <- arbitrary 
    frequency [(1, return $ LolNope), (3, return $ Yeppers a)]

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

possiblyTest :: IO () 
possiblyTest = do
    quickCheck (functorIdentity :: Possibly Int -> Bool)
    let c = functorCompose (*2) (+1) 
    let p x = c (x :: Possibly Int)
    quickCheck p
