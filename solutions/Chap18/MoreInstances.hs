module Chap18.MoreInstances where

import Test.QuickCheck 
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg 
  _ <*> _ = NopeDotJpg

instance Monad Nope where 
  return = pure 
  _ >>= _ = NopeDotJpg

instance Arbitrary a =>
        Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg 

instance Eq a =>
        EqProp (Nope a) where
  (=-=) = eq

type SSI = (String, String, Int)

trigNope :: Nope SSI
trigNope = undefined 

checkerNope :: IO () 
checkerNope = do
    quickBatch (functor trigNope)
    quickBatch (applicative trigNope)
    quickBatch (monad trigNope)

-- 2
data PhtEither b a = 
   Left' a
 | Right' b
 deriving (Eq, Show)

instance Functor (PhtEither b) where
  fmap f (Right' x) = Right' x
  fmap f (Left' x) = Left' (f x)

instance Applicative (PhtEither b) where
  pure = Left' 
  (Right' f) <*> _ = Right' f
  _ <*> Right' x = Right' x
  (Left' f) <*> (Left' x) = Left' (f x)

instance Monad (PhtEither b) where
  return = pure 
  (Right' x) >>= _ = Right' x
  (Left' x) >>= f = f x

instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (PhtEither b a) where
  arbitrary = eGen

instance (Eq a, Eq b) =>
        EqProp (PhtEither b a) where
  (=-=) = eq

eGen :: (Arbitrary a, Arbitrary b) => Gen (PhtEither b a)
eGen = do 
    a <- arbitrary 
    b <- arbitrary 
    frequency [(1, return $ Right' b), (3, return $ Left' a)]

trigEither :: PhtEither SSI SSI
trigEither = undefined

checkersEith :: IO () 
checkersEith = do 
    quickBatch (functor trigEither)
    quickBatch (applicative trigEither)
    quickBatch (monad trigEither)

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure 
  (Identity x) >>= f = f x 

instance Arbitrary a =>
        Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => 
        EqProp (Identity a) where
  (=-=) = eq

trigId :: Identity SSI
trigId = undefined 

checkersId :: IO ()
checkersId = do
    quickBatch (functor trigId)
    quickBatch (applicative trigId)
    quickBatch (monad trigId)

-- 4
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

instance Monad List where
  return = pure 
  Nil >>= _ = Nil 
  xs >>= f = concat' $ fmap f xs

instance Arbitrary a => 
        Arbitrary (List a) where
  arbitrary = liGen 

liGen :: Arbitrary a => Gen (List a)
liGen = do
  a <- arbitrary 
  frequency [(1, return Nil), (3, return $ Cons a Nil)]

instance Eq a =>
        EqProp (List a) where
  (=-=) = eq

trigList :: List SSI
trigList = undefined

checkersList :: IO () 
checkersList = do
    quickBatch (functor trigList)
    quickBatch (applicative trigList)
    quickBatch (monad trigList)
