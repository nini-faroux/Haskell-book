module C15_MonoidsSemis.OptionalMonoid where

import Test.QuickCheck

data Optional a = 
    Nada 
  | Only a 
  deriving (Eq, Show)

instance Monoid a =>
        Monoid (Optional a) where 
  mempty = Nada 
  mappend = (<>)

instance Semigroup a =>
        Semigroup (Optional a) where
  Nada <> x = x 
  x <> Nada = x 
  (Only x) <> (Only y) = Only (x <> y)

instance Arbitrary a => 
        Arbitrary (Optional a) where
  arbitrary = optGen

optGen :: Arbitrary a => Gen (Optional a) 
optGen = do
    a <- arbitrary 
    frequency [(1, return $ Nada), (3, return $ Only a)]

assoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
assoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

leftId :: (Eq m, Monoid m) => m -> Bool 
leftId a = a <> mempty == a

rightId :: (Eq m, Monoid m) => m -> Bool
rightId a = mempty <> a == a

type OptAssoc = Optional String -> Optional String -> Optional String -> Bool 
type FstId = Optional String -> Bool 

optTest :: IO () 
optTest = do
    quickCheck (assoc :: OptAssoc)
    quickCheck (leftId :: FstId)
    quickCheck (rightId :: FstId)
