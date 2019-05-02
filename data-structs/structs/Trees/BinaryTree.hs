{-# LANGUAGE InstanceSigs #-}

module Trees.BinaryTree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data BinTree a =
    Leaf 
  | Node (BinTree a) a (BinTree a)
  deriving (Eq, Show) 

instance Semigroup a => 
        Semigroup (BinTree a) where 
  Leaf <> ts = ts 
  ts <> Leaf = ts 
  (Node l x r) <> (Node l' x' r') = Node (l <> l') (x <> x') (r <> r')

instance Monoid a => 
        Monoid (BinTree a) where 
  mempty = Leaf 
  mappend = (<>)

instance Functor BinTree where 
  fmap _ Leaf = Leaf 
  fmap f (Node l x r) = Node (f <$> l) (f x) (f <$> r)

-- fails the interchange test 
instance Applicative BinTree where 
  pure x = Node Leaf x Leaf 

  Leaf <*> _ = Leaf
  _ <*> Leaf = Leaf 
  fs@(Node lf f rf) <*> (Node l x r) = Node (fs <*> l) (f x) (fs <*> r)

-- fails Left and Right Id tests 
-- this Binary Tree type only makes sense for Functor? 
-- not applicative and Monad
instance Monad BinTree where 
  return = pure 

  (>>=) :: BinTree a -> (a -> BinTree b) -> BinTree b
  Leaf >>= _ = Leaf 
  (Node l x r) >>= f = do
          l' <- l
          r' <- r 
          x' <- f x
          Node (f l') x' (f r')

conc :: BinTree a -> BinTree a -> BinTree a 
conc l r = undefined

foldTree :: (b -> a -> b) -> b -> BinTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l x r) = foldTree f (foldTree f z l `f` x) r

inOrder :: BinTree a -> [a] 
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

preOrder :: BinTree a -> [a] 
preOrder Leaf = [] 
preOrder (Node l x r) = [x] ++ preOrder l ++ preOrder r 

postOrder :: BinTree a -> [a] 
postOrder Leaf = []
postOrder (Node l x r) = postOrder l ++ postOrder r ++ [x] 

insert :: Ord a => a -> BinTree a -> BinTree a 
insert x Leaf = Node Leaf x Leaf 
insert x (Node l y r) 
  | x >= y    = Node l y (insert x r) 
  | otherwise = Node (insert x l) y r

-- testing
instance Eq a =>
        EqProp (BinTree a) where
  (=-=) = eq

instance Arbitrary a =>
        Arbitrary (BinTree a) where
  arbitrary = treeGen

treeGen :: Arbitrary a => Gen (BinTree a)
treeGen = do
  a <- arbitrary 
  b <- arbitrary 
  c <- arbitrary 
  oneof [return Leaf, return $ Node a b c]

type SSI = (String, String, Int)
type SSS = (String, String, String)

trigger :: BinTree SSI
trigger = undefined

trigger' :: BinTree SSS
trigger' = undefined

checkTree :: IO () 
checkTree = do 
  quickBatch (monoid trigger')
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (monad trigger)

treeMain :: IO () 
treeMain = sample (treeGen :: Gen (BinTree Int))
