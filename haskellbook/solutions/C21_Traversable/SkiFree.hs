{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module C21_Traversable.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => 
        Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => 
        EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor (S n) where
  fmap :: (a -> b) -> S n a -> S n b
  fmap f (S n y) = undefined

instance Foldable (S n) where
  foldr = undefined

instance Traversable n => 
    Traversable (S n) where
  traverse = undefined 
 
main = sample' (arbitrary :: Gen (S [] Int))
