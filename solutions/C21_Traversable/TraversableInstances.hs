{-# LANGUAGE InstanceSigs #-}

module C21_Traversable.TraversableInstances where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- 1
newtype Idy a = Idy a deriving (Eq, Show)

instance Functor Idy where
  fmap f (Idy x) = Idy (f x)

instance Applicative Idy where
  pure = Idy
  (Idy f) <*> (Idy x) = Idy (f x)

instance Foldable Idy where
  foldr f z (Idy x) = f x z

instance Traversable Idy where
  traverse :: (Applicative f) => (a -> f b) -> Idy a -> f (Idy b)
  traverse f (Idy x) = Idy <$> f x

instance Arbitrary a =>
        Arbitrary (Idy a) where
  arbitrary = Idy <$> arbitrary 

instance Eq a => 
        EqProp (Idy a) where
  (=-=) = eq

type SSS = (String, String, String)

trigIdy :: Idy SSS 
trigIdy = undefined

checkIdy :: IO () 
checkIdy = do
  quickBatch (functor trigIdy)
  quickBatch (applicative trigIdy)
  quickBatch (traversable trigIdy)

-- 2
newtype Const' a b = Const' { getConst :: a } deriving Show

instance Functor (Const' a) where
  fmap :: (a1 -> b) -> Const' a a1 -> Const' a b
  fmap f (Const' x) = Const' x

instance Monoid a =>
        Applicative (Const' a) where
  pure :: a1 -> Const' a a1
  pure _ = Const' mempty

  (<*>) :: Const' a (a1 -> b) -> Const' a a1 -> Const' a b 
  (Const' f) <*> (Const' x) = Const' $ f <> x

instance Foldable (Const' a) where
  foldr :: (a1 -> b -> b) -> b -> Const' a a1 -> b 
  foldr _ z (Const' _) = z

instance Traversable (Const' a) where
  traverse :: Applicative  f => (a1 -> f b) -> Const' a a1 -> f (Const' a b)
  traverse f (Const' x) = pure $ Const' x

instance Arbitrary a => 
        Arbitrary (Const' a b) where
  arbitrary = Const' <$> arbitrary 

instance (Eq a, EqProp a) => 
        EqProp (Const' a b) where
  (Const' x) =-= (Const' y) = x =-= y

trigConst :: Const' SSS SSS
trigConst = undefined

checkConst :: IO () 
checkConst = do 
    quickBatch (functor trigConst)
    quickBatch (applicative trigConst)
    quickBatch (traversable trigConst)

-- 3
data Opt a = 
    Nay 
  | Yes a 
  deriving (Eq, Show)

instance Functor Opt where
  fmap _ Nay = Nay
  fmap f (Yes x) = Yes $ f x

instance Applicative Opt where
  pure = Yes 
  Nay <*> _ = Nay 
  _ <*> Nay = Nay 
  (Yes f) <*> (Yes x) = Yes $ f x

instance Foldable Opt where
  foldr :: (a -> b -> b) -> b -> Opt a -> b
  foldr _ z Nay = z 
  foldr f z (Yes x) = f x z

instance Traversable Opt where
  traverse :: Applicative f => (a -> f b) -> Opt a -> f (Opt b) 
  traverse _ Nay = pure Nay
  traverse f (Yes x) = Yes <$> f x

instance Arbitrary a => 
        Arbitrary (Opt a) where
  arbitrary = optGen

optGen :: Arbitrary a => Gen (Opt a) 
optGen = do 
  a <- arbitrary 
  frequency [(1, return Nay), (3, return $ Yes a)]

instance Eq a => 
        EqProp (Opt a) where
  (=-=) = eq

trigOpt :: Opt SSS
trigOpt = undefined

checkOpt :: IO () 
checkOpt = do
  quickBatch (functor trigOpt)
  quickBatch (applicative trigOpt) 
  quickBatch (traversable trigOpt)

-- 4
data Listy a =
    Empty' 
  | Consy a (Listy a)
  deriving (Eq, Show)

instance Functor Listy where
  fmap :: (a -> b) -> Listy a -> Listy b
  fmap _ Empty' = Empty' 
  fmap f (Consy x xs) = Consy (f x) (fmap f xs)

instance Applicative Listy where
  pure x = Consy x Empty'

  Empty' <*> _ = Empty' 
  _ <*> Empty' = Empty' 
  fs <*> xs = go fs xs Empty'
    where
      go Empty' _ acc = acc
      go (Consy f fs) Empty' acc = go fs xs acc
      go (Consy f fs) (Consy x xs) acc = go (Consy f fs) xs (appendVal (f x) acc)

appendVal :: a -> Listy a -> Listy a
appendVal x Empty' = Consy x Empty'
appendVal x (Consy y ys) = Consy y (appendVal x ys)

instance Foldable Listy where
  foldr _ z Empty' = z
  foldr f z (Consy x xs) = f x (foldr f z xs)

instance Traversable Listy where
  traverse :: Applicative f => (a -> f b) -> Listy a -> f (Listy b)
  traverse _ Empty' = pure Empty'
  traverse f (Consy x xs) = Consy <$> f x <*> traverse f xs
  
instance Arbitrary a => 
        Arbitrary (Listy a) where
  arbitrary = liGen 

liGen :: Arbitrary a => Gen (Listy a) 
liGen = do 
  a <- arbitrary 
  frequency [(1, return Empty'), (3, return $ Consy a Empty')]

instance Eq a => 
        EqProp (Listy a) where
  (=-=) = eq

trigList :: Listy SSS
trigList = undefined

checkListy :: IO () 
checkListy = do
  quickBatch (functor trigList)
  quickBatch (applicative trigList)
  quickBatch (traversable trigList)

-- 5
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) =>
        Applicative (Three a b) where
  pure = Three mempty mempty 
  (Three f g h) <*> (Three a b c) = Three (f <> a) (g <> b) (h c)

instance Foldable (Three a a1) where
  foldr :: (a2 -> b -> b) -> b -> Three a a1 a2 -> b
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a a1) where
  traverse :: Applicative f => (a2 -> f b) -> Three a a1 a2 -> f (Three a a1 b)
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
        Arbitrary (Three a b c) where
  arbitrary = threeGen

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c) 
threeGen = do
  a <- arbitrary
  b <- arbitrary 
  Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => 
    EqProp (Three a b c) where
  (=-=) = eq

trigThree :: Three SSS SSS SSS
trigThree = undefined

checkThree :: IO () 
checkThree = do
  quickBatch (functor trigThree)
  quickBatch (applicative trigThree)
  quickBatch (traversable trigThree)

-- 6 
data Pair'' a b = Pair'' a b deriving (Show, Eq) 

instance Functor (Pair'' a) where
  fmap :: (a1 -> b) -> Pair'' a a1 -> Pair'' a b
  fmap f (Pair'' a b) = Pair'' a (f b)

instance Monoid a =>
        Applicative (Pair'' a) where
  pure = Pair'' mempty
  (Pair'' f g) <*> (Pair'' x y) = Pair'' (f <> x) (g y)

instance Foldable (Pair'' a) where
  foldr :: (a1 -> b -> b) -> b -> Pair'' a a1 -> b
  foldr f z (Pair'' x y) = f y z

instance Traversable (Pair'' a) where
  traverse :: Applicative f => (a1 -> f b) -> Pair'' a a1 -> f (Pair'' a b)
  traverse f (Pair'' x y) = Pair'' x <$> f y 

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Pair'' a b) where
  arbitrary = pairGen 

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair'' a b)
pairGen = do
  a <- arbitrary 
  Pair'' a <$> arbitrary 

instance (Eq a, Eq b) => 
        EqProp (Pair'' a b) where
  (=-=) = eq

trigPair :: Pair'' SSS SSS 
trigPair = undefined 

checkPair :: IO () 
checkPair = do 
  quickBatch (functor trigPair)
  quickBatch (applicative trigPair)
  quickBatch (traversable trigPair)

-- 7 
data Big a b = Big a b b deriving (Show, Eq) 

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Monoid a => 
        Applicative (Big a) where
  pure x = Big mempty x x
  (Big f g h) <*> (Big x y z) = Big (f <> x) (g y) (h z)

instance Foldable (Big a) where
  foldr :: (a1 -> b -> b) -> b -> Big a a1 -> b
  foldr f z (Big a b c) = f c z

  foldMap :: Monoid m => (a1 -> m) -> Big a a1 -> m
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  traverse :: Applicative f => (a1 -> f b) -> Big a a1 -> f (Big a b)
  traverse f (Big a b c) = Big a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Big a b) where
  arbitrary = bigGen

bigGen :: (Arbitrary a, Arbitrary b) => Gen (Big a b) 
bigGen = do
  a <- arbitrary
  b <- arbitrary 
  return $ Big a b b

instance (Eq a, Eq b) =>
        EqProp (Big a b) where
  (=-=) = eq

trigBig :: Big SSS SSS
trigBig = undefined

checkBig :: IO () 
checkBig = do
  quickBatch (functor trigBig)
  quickBatch (applicative trigBig)
  quickBatch (traversable trigBig)

-- 8
data Bigger a b = Bigger a b b b deriving (Show, Eq) 

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Monoid a =>
        Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (Bigger f g h i) <*> (Bigger a b c d) = Bigger (f <> a) (g b) (h c) (i d)

instance Foldable (Bigger a) where
  foldr :: (a1 -> b -> b) -> b -> Bigger a a1 -> b
  foldr f z (Bigger a b c d) = f d z

  foldMap :: Monoid m => (a1 -> m) -> Bigger a a1 -> m
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where 
  traverse :: Applicative f => (a1 -> f b) -> Bigger a a1 -> f (Bigger a b)
  traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Bigger a b) where
  arbitrary = biggerGen 

biggerGen :: (Arbitrary a, Arbitrary b) => Gen (Bigger a b) 
biggerGen = do
    a <- arbitrary
    b <- arbitrary
    return $ Bigger a b b b

instance (Eq a, Eq b) => 
        EqProp (Bigger a b) where
  (=-=) = eq

trigBigger :: Bigger SSS SSS
trigBigger = undefined

checkBigger :: IO () 
checkBigger = do
  quickBatch (functor trigBigger)
  quickBatch (applicative trigBigger)
  quickBatch (traversable trigBigger)
