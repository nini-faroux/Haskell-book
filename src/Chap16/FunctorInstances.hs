module Chap16.FunctorInstances where
import Test.QuickCheck

-- 1
newtype Identityy a = Identityy a deriving (Eq, Show)

instance Functor Identityy where
  fmap f (Identityy x) = Identityy (f x)

instance Arbitrary a => 
        Arbitrary (Identityy a) where
  arbitrary = identGen

identGen :: Arbitrary a => Gen (Identityy a)
identGen = do
    a <- arbitrary 
    return (Identityy a)

-- 2
data Pairr a = Pairr a a deriving (Eq, Show)

instance Functor Pairr where 
  fmap f (Pairr x y) = Pairr (f x) (f y)

instance Arbitrary a => 
        Arbitrary (Pairr a) where
  arbitrary = pairGen

pairGen :: Arbitrary a => Gen (Pairr a)
pairGen = do
    a <- arbitrary 
    return (Pairr a a)

-- 3
data Twoo a b = Twoo a b deriving (Eq, Show)

instance Functor (Twoo a) where
  fmap f (Twoo x y) = Twoo x (f y)

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Twoo a b) where
  arbitrary = twoGen 

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Twoo a b)
twoGen = do
    a <- arbitrary 
    b <- arbitrary 
    return (Twoo a b)

-- 4
data Threee a b c = Threee a b c deriving (Eq, Show) 

instance Functor (Threee a b) where
  fmap f (Threee x y z) = Threee x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
        Arbitrary (Threee a b c) where
  arbitrary = tGen 

tGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Threee a b c)
tGen = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return (Threee a b c)

-- 5
data Threee' a b = Threee' a b b deriving (Eq, Show)

instance Functor (Threee' a) where
  fmap f (Threee' x y z) = Threee' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Threee' a b) where
  arbitrary = threeGen 

threeGen :: (Arbitrary a, Arbitrary b) => Gen (Threee' a b) 
threeGen = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return (Threee' a b c)

-- 6 
data Fourr a b c d = Fourr a b c d deriving (Eq, Show)

instance Functor (Fourr a b c) where
  fmap f (Fourr a b c d) = Fourr a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
        Arbitrary (Fourr a b c d) where
  arbitrary = fourGen 

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Fourr a b c d)
fourGen = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    d <- arbitrary 
    return (Fourr a b c d)

-- 7 
data Fourr' a b = Fourr' a a a b deriving (Eq, Show)

instance Functor (Fourr' a) where 
  fmap f (Fourr' a b c d) = Fourr' a b c (f d)

instance (Arbitrary a, Arbitrary b) => 
        Arbitrary (Fourr' a b) where
  arbitrary = fourrGen 

fourrGen :: (Arbitrary a, Arbitrary b) => Gen (Fourr' a b)
fourrGen = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    d <- arbitrary 
    return (Fourr' a b c d)

-- QuickCheck 
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorTest :: IO () 
functorTest = do
    -- Fourr'
    let comp = functorCompose (*2) (+1)
    let frC x = comp (x :: Fourr' Int Double)
    quickCheck (functorIdentity :: Fourr' String Int -> Bool)
    quickCheck frC
     
    -- Fourr 
    let c4' = functorCompose (*2) (+1)
    let fC x = c4' (x :: Fourr Int Int Int Double) 
    quickCheck (functorIdentity :: Fourr Int Int Int Double -> Bool) 
    quickCheck fC

    -- Threee'
    let c3 = functorCompose (*2) (+1)
    let tC x = c3 (x :: Threee' Int Double)
    quickCheck (functorIdentity :: Threee' Int Double -> Bool)
    quickCheck tC

    -- Threee
    let c3' = functorCompose (*2) (+1) 
    let tC' x = c3' (x :: Threee Int Int Double)
    quickCheck (functorIdentity :: Threee Int Int Double -> Bool)
    quickCheck tC'

    -- Twoo 
    let c2 = functorCompose (*2) (+1) 
    let twoC x = c2 (x :: Twoo Int Double)
    quickCheck (functorIdentity :: Twoo Int Double -> Bool)
    quickCheck twoC

    -- Pairr 
    let cP = functorCompose (*2) (+1) 
    let pC x = cP (x :: Pairr Int)
    quickCheck (functorIdentity :: Pairr Int -> Bool)
    quickCheck pC

    -- Identity 
    let cI = functorCompose (*2) (+1)
    let iC x = cI (x :: Identityy Int)
    quickCheck (functorIdentity :: Identityy Int -> Bool)
    quickCheck iC
