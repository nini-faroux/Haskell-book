{-# LANGUAGE InstanceSigs #-}

module Chap18.EitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = 
    Firstt a
  | Secondd b
  deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap f (Firstt x) = Firstt x 
  fmap f (Secondd x) = Secondd (f x)

instance Applicative (Sum a) where 
  pure = Secondd
  (Firstt f) <*> _ = Firstt f
  _ <*> (Firstt x) = Firstt x
  (Secondd f) <*> (Secondd x) = Secondd (f x)

instance Monad (Sum a) where 
  return = pure
  (>>=) :: Sum a a1 -> (a1 -> Sum a b) -> Sum a b
  (Firstt x) >>= _ = Firstt x
  (Secondd x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Sum a b) where
  arbitrary = sumGen 

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
    a <- arbitrary 
    b <- arbitrary 
    frequency [(1, return $ Firstt a), (3, return $ Secondd b)]

instance (Eq a, Eq b) =>
        EqProp (Sum a b) where
  (=-=) = eq

type ISI = (Int, String, Int)

trigger :: Sum ISI ISI
trigger = undefined

checkerEmon :: IO () 
checkerEmon = do
    quickBatch $ functor trigger 
    quickBatch $ applicative trigger 
    quickBatch $ monad trigger
