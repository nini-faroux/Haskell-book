module Chap17.MoreInstances where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-- Testing 
instance Eq a =>
        EqProp (Pair a) where
  (=-=) = eq

instance Arbitrary a => 
        Arbitrary (Pair a) where
  arbitrary = pGen

pGen :: Arbitrary a => Gen (Pair a)
pGen = do
    a <- arbitrary
    return $ Pair a a

type SSI = (String, String, Int)

trigPair :: Pair SSI
trigPair = undefined

pairChecker :: IO () 
pairChecker = quickBatch (applicative trigPair)

-- 2
data Two'' a b = Two'' a b deriving (Eq, Show)

instance Functor (Two'' a) where
  fmap f (Two'' x y) = Two'' x (f y)

instance Monoid a =>
        Applicative (Two'' a) where
  pure = Two'' mempty 
  (Two'' f g) <*> (Two'' x y) = Two'' (f <> x) (g y)

instance (Eq a, Eq b) =>
        EqProp (Two'' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (Two'' a b) where
  arbitrary = tGen 

tGen :: (Arbitrary a, Arbitrary b) => Gen (Two'' a b)
tGen = do
    a <- arbitrary 
    b <- arbitrary 
    return $ Two'' a b

type SSS = (String, String, String)

trigTwo'' :: Two'' SSS SSS 
trigTwo'' = undefined

twoChecker :: IO ()
twoChecker = quickBatch (applicative trigTwo'')

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => 
        Applicative (Three a b) where
  pure = Three mempty mempty
  (Three f g h) <*> (Three x y z) = Three (f <> x) (g <> y) (h z)

instance (Eq a, Eq b, Eq c) => 
        EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
        Arbitrary (Three a b c) where
  arbitrary = thGen

thGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
thGen = do 
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary
    return $ Three a b c

trigThree :: Three SSS SSS SSS
trigThree = undefined

threeChecker :: IO () 
threeChecker = quickBatch (applicative trigThree)

-- 4 
data Three' a b = Three' a b b deriving (Show, Eq) 

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) =>
        Applicative (Three' a) where
  pure x = Three' mempty x x 
  (Three' f g h) <*> (Three' x y z) = Three' (f <> x) (g y) (h z)

instance (Eq a, Eq b) =>
        EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (Three' a b) where
  arbitrary = thrGen

thrGen :: (Arbitrary a, Arbitrary b) => Gen (Three' a b) 
thrGen = do
    a <- arbitrary 
    b <- arbitrary 
    return $ Three' a b b

trigThree' :: Three' SSS SSS 
trigThree' = undefined

threeChecker' :: IO () 
threeChecker' = quickBatch (applicative trigThree')

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => 
        Applicative (Four a b c) where
  pure = Four mempty mempty mempty 
  (Four f g h i) <*> (Four a b c d) = Four (f <> a) (g <> b) (h <> c) (i d)

instance (Eq a, Eq b, Eq c, Eq d) => 
        EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
  arbitrary = fourGen 

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary 
    d <- arbitrary 
    return $ Four a b c d

trigFour :: Four SSS SSS SSS SSS
trigFour = undefined

fourChecker :: IO () 
fourChecker = quickBatch (applicative trigFour)

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) =>
        Applicative (Four' a) where
  pure = Four' mempty mempty mempty 
  (Four' f g h i) <*> (Four' a b c d) = Four' (f <> a) (g <> b) (h <> c) (i d)

instance (Eq a, Eq b) => 
        EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Four' a b) where
  arbitrary = fourGen'

fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b) 
fourGen' = do
    a <- arbitrary 
    b <- arbitrary 
    return $ Four' a a a b

trigFour' :: Four' SSS SSS
trigFour' = undefined

fourChecker' :: IO () 
fourChecker' = quickBatch (applicative trigFour')
