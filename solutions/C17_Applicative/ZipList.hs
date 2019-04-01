module C17_Applicative.ZipList where

import Control.Applicative 
import Data.Monoid 
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes 

data Listz a = 
    Nilz
  | Conz a (Listz a)
  deriving (Eq, Show)

instance Functor Listz where
   fmap _ Nilz = Nilz
   fmap f (Conz x xs) = Conz (f x) (fmap f xs)

instance Applicative Listz where
  pure x = Conz x Nilz

  Nilz <*> _ = Nilz
  _ <*> Nilz = Nilz
  --(Conz f fs) <*> (Conz x xs) = Conz (f x) (fs <*> xs)
  --(Conz f fs) <*> xs = appendList (flatMap' (\x -> Conz (f x) Nilz) xs) (fs <*> xs)
  xs <*> ys = go xs ys Nilz 
    where
      go Nilz _ acc = acc 
      go _ Nilz acc = acc 
      go (Conz f Nilz) (Conz x xs) acc = go (Conz f Nilz) xs (appendVal (f x) acc)
      go (Conz f fs) (Conz x xs) acc = go fs xs (appendVal (f x) acc)

appendVal :: a -> Listz a -> Listz a
appendVal x Nilz = Conz x Nilz
appendVal x (Conz y ys) = Conz y (appendVal x ys)

appendList :: Listz a -> Listz a -> Listz a
appendList Nilz ys = ys
appendList (Conz x xs) ys = Conz x $ appendList xs ys

foldList :: (a -> b -> b) -> b -> Listz a -> b
foldList _ z Nilz = z
foldList f z (Conz x xs) = f x (foldList f z xs)

concat' :: Listz (Listz a) -> Listz a
concat' = foldList appendList Nilz

flatMap' :: (a -> Listz b) -> Listz a -> Listz b
flatMap' _ Nilz = Nilz
flatMap' f xs = concat' $ fmap f xs

newtype ZipListz a = 
  ZipListz (Listz a) 
  deriving (Eq, Show)

instance Functor ZipListz where
  fmap f (ZipListz xs) = ZipListz $ fmap f xs 

instance Applicative ZipListz where
  pure x = ZipListz $ pure x 
  
  (ZipListz Nilz) <*> _ = ZipListz Nilz 
  _ <*> (ZipListz Nilz) = ZipListz Nilz 
  (ZipListz xs) <*> (ZipListz ys) = ZipListz $ xs <*> ys

-- Testing
instance Eq a => 
        EqProp (ZipListz a) where
  xs =-= ys = xs' `eq` ys' 
    where xs' = let (ZipListz l) = xs
                in take' 3000 l 
          ys' = let (ZipListz l) = ys 
                in take' 3000 l

take' :: Int -> Listz a -> Listz a 
take' _ Nilz = Nilz
take' n (Conz x xs) = Conz x (take' (n-1) xs)

instance Eq a =>
        EqProp (Listz a) where
  (=-=) = eq

instance Arbitrary a =>
        Arbitrary (Listz a) where
  arbitrary = lisGen

lisGen :: Arbitrary a => Gen (Listz a)
lisGen = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Nilz), (3, return $ Conz a b)]

type SSI = (String, String, Int)

instance Arbitrary a => 
        Arbitrary (ZipListz a) where
  arbitrary = ZipListz <$> arbitrary

checkerListz :: IO ()
checkerListz = do
    let zxs = ZipListz $ Conz ("b", "w", 1 :: Int) Nilz
    quickBatch $ applicative zxs
