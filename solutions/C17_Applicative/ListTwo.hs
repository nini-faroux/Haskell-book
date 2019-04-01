module C17_Applicative.ListTwo where

import Test.QuickCheck 
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Listt' a =
    Nill'
  | Conss' a (Listt' a)
  deriving (Eq, Show)

instance Functor Listt' where
  fmap _ Nill' = Nill'
  fmap f (Conss' x xs) = Conss' (f x) (fmap f xs)

-- (<*>) without pattern matching
instance Applicative Listt' where
  pure x = Conss' x Nill'

  Nill' <*> _ = Nill'
  _ <*> Nill' = Nill'
  (Conss' f fs) <*> xs = appendList (flatMap' (\x -> Conss' (f x) Nill') xs) (fs <*> xs)

appendList :: Listt' a -> Listt' a -> Listt' a
appendList Nill' ys = ys
appendList (Conss' x xs) ys = Conss' x $ appendList xs ys

foldList :: (a -> b -> b) -> b -> Listt' a -> b
foldList _ z Nill' = z
foldList f z (Conss' x xs) = f x (foldList f z xs)

concat' :: Listt' (Listt' a) -> Listt' a
concat' = foldList appendList Nill'

flatMap' :: (a -> Listt' b) -> Listt' a -> Listt' b
flatMap' _ Nill' = Nill'
flatMap' f xs = concat' $ fmap f xs

-- Testing 
instance Eq a => 
        EqProp (Listt' a) where
  (=-=) = eq

instance Arbitrary a => 
        Arbitrary (Listt' a) where
  arbitrary = lissGen 

lissGen :: Arbitrary a => Gen (Listt' a) 
lissGen = do
    a <- arbitrary 
    b <- arbitrary
    frequency [(1, return Nill'), (3, return $ Conss' a b)]

type SSI = (String, String, Int)

trigger :: Listt' SSI
trigger = undefined

checkList :: IO () 
checkList = quickBatch (applicative trigger)
