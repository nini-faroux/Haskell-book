module Chap20.LibraryFuncs where

import Data.Foldable (foldMap)
import Data.Monoid 
import Data.Maybe (fromJust, isNothing)

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum 

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product 

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\x y -> (x == a) || y) False 

-- 4
minimum' :: (Foldable t, Functor t, Ord a) => t a -> Maybe a 
minimum' xs = foldr (\x y -> if min' x y == x then x else y) (head' xs) (Just <$> xs)

min' :: Ord a => Maybe a -> Maybe a -> Maybe a 
min' x y 
  | fromJust x < fromJust y = x 
  | otherwise = y

head' :: Foldable t => t a -> Maybe a
head' = foldr (\x _ -> Just x) Nothing 

-- 5
maximum' :: (Foldable t, Functor t, Ord a) => t a -> Maybe a
maximum' xs = foldr (\x y -> if max' x y == x then x else y) (head' xs) (Just <$> xs)

max' :: Ord a => Maybe a -> Maybe a -> Maybe a
max' x y 
  | fromJust x > fromJust y = x
  | otherwise = y

-- 6
null' :: (Foldable t) => t a -> Bool 
null' = foldr (\_ _ -> False) True 

-- need to specify semigroup type.. 
-- $ null'' ([] :: [String])
null'' :: (Foldable t, Semigroup a) => t a -> Bool
null'' xs 
  | isNothing (foldMap Just xs) = True 
  | otherwise = False

-- 7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ y -> 1 + y) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\x y -> f x <> y) mempty 
