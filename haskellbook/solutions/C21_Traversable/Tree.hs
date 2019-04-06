{-# LANGUAGE InstanceSigs #-}

module C21_Traversable.Tree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Tree a =
    Empty 
  | Leaf a 
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty 
  fmap f (Leaf x) = Leaf (f x) 
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l x r) = f x (foldr f (foldr f z l) r)

  foldMap :: Monoid m => (a -> m) -> Tree a -> m 
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => 
        Arbitrary (Tree a) where
  arbitrary = treeGen 

treeGen :: Arbitrary a => Gen (Tree a) 
treeGen = do
    a <- arbitrary 
    frequency [(1, return Empty), (3, return $ Leaf a), (3, return $ Node (Leaf a) a (Leaf a))]

instance Eq a =>
        EqProp (Tree a) where
  (=-=) = eq

type SSS = (String, String, String)

trigTree :: Tree SSS
trigTree = undefined

checkTree :: IO () 
checkTree = do
    quickBatch (functor trigTree)
    quickBatch (traversable trigTree)
