module C17_Applicative.Validation where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Valid e a = 
    Fail e
  | Joy a
  deriving (Eq, Show)

instance Functor (Valid e) where
  fmap _ (Fail x) = Fail x 
  fmap f (Joy x) = Joy (f x)

instance Monoid e => 
    Applicative (Valid e) where
  pure = Joy

  (Fail f) <*> (Fail x) = Fail (f <> x)
  (Fail f) <*> _ = Fail f
  (Joy f) <*> (Joy x) = Joy (f x)
  (Joy f) <*> (Fail x) = Fail x

instance (Eq e, Eq a) => 
        EqProp (Valid e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) =>
        Arbitrary (Valid e a) where
  arbitrary = valGen 

valGen :: (Arbitrary e, Arbitrary a) => Gen (Valid e a) 
valGen = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return $ Fail e), (3, return $ Joy a)]

type SSS = (String, String, String)

trigValid :: Valid SSS SSS
trigValid = undefined

validChecker :: IO () 
validChecker = quickBatch (applicative trigValid)
