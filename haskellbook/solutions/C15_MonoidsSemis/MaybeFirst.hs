module C15_MonoidsSemis.MaybeFirst where

import Test.QuickCheck

data Optional' a =
    Nix
  | Only' a
  deriving (Eq, Show)

newtype First' a = 
    First' { getFirst :: Optional' a } 
    deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nix 
  mappend = (<>)
  
instance Semigroup (First' a) where
  (First' (Only' x)) <> _ = (First' (Only' x))
  _ <> (First' (Only' x)) = (First' (Only' x))
  _ <> _ = First' Nix

-- tests 
instance Arbitrary a => 
        Arbitrary (First' a) where
  arbitrary = firstGen  

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
    a <- arbitrary 
    frequency [(1, return $ First' Nix), (3, return $ First' (Only' a))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend 

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FirstId = First' String -> Bool 

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

mainFirst :: IO () 
mainFirst = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FirstId) 
    quickCheck (monoidRightIdentity :: FirstId) 
