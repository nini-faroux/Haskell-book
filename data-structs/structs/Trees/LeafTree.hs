{-# LANGUAGE InstanceSigs #-}

module Trees.LeafTree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data LTree a = 
    Leaf a
  | LNode (LTree a) (LTree a)
  deriving (Eq, Show)

instance Functor LTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (LNode l r) = LNode (f <$> l) (f <$> r)

instance Applicative LTree where 
  pure = Leaf
  
  (<*>) = undefined

instance Monad LTree where 
  return = pure 

  (>>=) :: LTree a -> (a -> LTree b) -> LTree b
  (Leaf x) >>= f = f x 
  (LNode l r) >>= f = LNode (l >>= \x -> f x) (r >>= \x -> f x)

-- testing 
instance Eq a =>
        EqProp (LTree a) where 
  (=-=) = eq

instance Arbitrary a =>
        Arbitrary (LTree a) where
  arbitrary = ltreeGen 

ltreeGen :: Arbitrary a => Gen (LTree a) 
ltreeGen = do
  a <- arbitrary 
  b <- arbitrary 
  c <- arbitrary
  oneof [return $ Leaf a, return $ LNode b c]

type SSI = (String, String, Integer)

trigger :: LTree SSI
trigger = undefined

checkLTree :: IO () 
checkLTree = do
  quickBatch (functor trigger)
  quickBatch (monad trigger)
