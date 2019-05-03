{-# LANGUAGE InstanceSigs #-}

module Trees.TreeL where 

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers 

data TreeL a = 
  NodeL a [TreeL a] 
  deriving (Eq, Show)

instance Functor TreeL where
  fmap f (NodeL x []) = NodeL (f x) []
  fmap f (NodeL x xs) = NodeL (f x) (fmap f <$> xs)

-- testing 
instance Eq a =>
        EqProp (TreeL a) where
  (=-=) = eq

instance Arbitrary a =>
        Arbitrary (TreeL a) where 
  arbitrary = treelGen

treelGen :: Arbitrary a => Gen (TreeL a) 
treelGen = do
  a <- arbitrary 
  b <- arbitrary 
  oneof [return $ NodeL a [], return $ NodeL a [b]]

type SSI = (String, String, Int) 

trigger :: TreeL SSI 
trigger = undefined 

checkTreeL :: IO () 
checkTreeL = quickBatch (functor trigger)

treeLMain :: IO () 
treeLMain = do
  let t1 = NodeL 1 [NodeL 2 [NodeL 4 [NodeL 8 []]]]
  print t1
  print $ (*2) <$> t1
  sample (treelGen :: Gen (TreeL Int))
