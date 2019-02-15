module Chap15.MonoidSgInstances where

import Data.Monoid (Sum, Product, Any)
import Test.QuickCheck

-- Monoid and Semigroup Instances 
-- 8 
newtype Mem s a = Mem { runMem :: s -> (a, s) } 

instance Monoid a => 
        Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, id s)
  mappend = (<>)

instance Semigroup a =>
        Semigroup (Mem s a) where
  f <> g = Mem $ \s -> ((fst $ runMem f s) <> (fst $ runMem g s), snd $ runMem g (snd $ runMem f s))

memTest = do
  let f' = Mem $ \s -> ("hi", s + 1)
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0

-- 7
newtype Comp a = Comp { unComp :: (a -> a) } 

instance Monoid a => 
        Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

instance Semigroup a =>
        Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ \x -> f (g x)

instance (Arbitrary a, CoArbitrary a) => 
        Arbitrary (Comp a) where
  arbitrary = compGen 

compGen :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
compGen = do
    a <- arbitrary 
    return (Comp a)

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool 

-- 6
newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance Monoid b => 
        Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty
  mappend = (<>)

instance Semigroup b => 
        Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

instance (Arbitrary a, Arbitrary b, CoArbitrary a) =>
        Arbitrary (Combine a b) where
  arbitrary = combGen 

combGen :: (CoArbitrary a, Arbitrary a, Arbitrary b) => Gen (Combine a b)
combGen = do
    f <- arbitrary 
    return (Combine f)

type CombAssoc = Combine String String -> Combine String String -> Combine String String -> Bool 

-- 5 
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True 
  _ <> (BoolDisj True) = BoolDisj True 
  _ <> _               = BoolDisj False

instance Arbitrary BoolDisj where 
  arbitrary = disjGen 

disjGen :: Gen BoolDisj 
disjGen = frequency [(1, return $ BoolDisj False), (3, return $ BoolDisj True)]

type BdisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool 
type DisjId = BoolDisj -> Bool

disjTest :: IO () 
disjTest = do 
    quickCheck (semigroupAssoc :: BdisjAssoc) 
    quickCheck (monoidLeftId :: DisjId)
    quickCheck (monoidRightId :: DisjId)

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj False 
  mappend = (<>)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False 
  _ <> (BoolConj False) = BoolConj False 
  _ <> _                = BoolConj True 

instance Arbitrary BoolConj where
  arbitrary = bconjGen

bconjGen :: Gen BoolConj
bconjGen = frequency [(1, return $ BoolConj False), (1, return $ BoolConj False)]

type BconjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool 
type BconjId = BoolConj -> Bool

bconjTest :: IO () 
bconjTest = do
    quickCheck (semigroupAssoc :: BconjAssoc)
    quickCheck (monoidLeftId :: BconjId) 
    quickCheck (monoidRightId :: BconjId)

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => 
        Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Semigroup a, Semigroup b) => 
        Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Two a b) where 
  arbitrary = twoGen 

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
    a <- arbitrary 
    b <- arbitrary 
    return (Two a b)

type TwoAssoc = Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool
type TwoId = Two String (Sum Int) -> Bool

twoTest :: IO () 
twoTest = do
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidRightId :: TwoId)
    quickCheck (monoidLeftId :: TwoId)

-- 2, Identity' 
newtype Identity' a = Identity' a deriving (Eq, Show)

instance Monoid a => 
        Monoid (Identity' a) where
  mempty = Identity' mempty
  mappend = (<>)

instance Semigroup a => 
        Semigroup (Identity' a) where
  (Identity' x) <> (Identity' y) = Identity' (x <> y)

instance Arbitrary a => 
        Arbitrary (Identity' a) where
  arbitrary = idGen

idGen :: Arbitrary a => Gen (Identity' a)
idGen = do
    a <- arbitrary 
    return (Identity' a)

type IdAssoc = Identity' String -> Identity' String -> Identity' String -> Bool
type IdId = Identity' String -> Bool

identTest :: IO () 
identTest = do
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (monoidRightId :: IdId)
    quickCheck (monoidLeftId :: IdId)

-- 1, Trivial 
data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial 
  mappend = (<>)

instance Semigroup Trivial where
  Trivial <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial 

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool 
type TrivId = Trivial -> Bool

trivTest :: IO () 
trivTest = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftId :: TrivId)
    quickCheck (monoidRightId :: TrivId)

-- Just Semigroup Instances 
-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => 
        Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
        Arbitrary (Three a b c) where 
  arbitrary = threeGen 

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return (Three a b c)

type ThreeAssoc = Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Three String (Sum Int) (Product Int) -> Bool 

threeTest :: IO () 
threeTest = do
    quickCheck (semigroupAssoc :: ThreeAssoc)

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
        Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
        Arbitrary (Four a b c d) where
  arbitrary = fourGen 

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary 
    d <- arbitrary 
    return (Four a b c d)

type FourAssoc = Four String (Sum Int) (Product Int) String -> Four String (Sum Int) (Product Int) String -> Four String (Sum Int) (Product Int) String -> Bool 

fourTest :: IO () 
fourTest = do
    quickCheck (semigroupAssoc :: FourAssoc)

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x 
  _ <> (Snd x) = Snd x 
  _ <> (Fst x) = Fst x

instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (Or a b) where
  arbitrary = orGen 

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b) 
orGen = do
    a <- arbitrary
    b <- arbitrary 
    frequency [(1, return $ Fst a), (3, return $ Snd b)]

type OrAssoc = Or String (Sum Int) -> Or String (Sum Int) -> Or String (Sum Int) -> Bool 

orTest :: IO () 
orTest = quickCheck (semigroupAssoc :: OrAssoc)

-- 11
data Validation a b = 
    Failure' a 
  | Success' b
  deriving (Eq, Show)

instance Semigroup a =>
        Semigroup (Validation a b) where
  (Success' a) <> _ = Success' a
  _ <> (Success' b) = Success' b
  (Failure' x) <> (Failure' y) = Failure' $ x <> y

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Validation a b) where
  arbitrary = validGen

validGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validGen = do
    a <- arbitrary 
    b <- arbitrary 
    frequency [(2, return $ Failure' a), (3, return $ Failure' b)]

type ValidAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

validTest :: IO () 
validTest = quickCheck (semigroupAssoc :: ValidAssoc)

-- QuickCheck abstractions

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool 
monoidLeftId a = (a <> mempty) == a

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = (mempty <> a) == a
