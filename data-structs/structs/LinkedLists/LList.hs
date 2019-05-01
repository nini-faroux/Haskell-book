{-# LANGUAGE InstanceSigs #-}

module LinkedLists.LList where 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = 
    Nil 
  | Cons a (List a) 
  deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = appendList (flatMap' (\x -> Cons (f x) Nil) xs) (fs <*> xs)

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x $ appendList xs ys

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ z Nil = z
foldList f z (Cons x xs) = f x (foldList f z xs)

concat' :: List (List a) -> List a
concat' = foldList appendList Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' _ Nil = Nil
flatMap' f xs = concat' $ fmap f xs


-- Testing 
instance Eq a => 
        EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => 
        Arbitrary (List a) where
  arbitrary = listGen 

listGen :: Arbitrary a => Gen (List a) 
listGen = do
    a <- arbitrary 
    b <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a b)]

type SSI = (String, String, Int)

trigger :: List SSI
trigger = undefined

checkList :: IO () 
checkList = quickBatch (applicative trigger)
