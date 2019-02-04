module Chap17.Constant where

newtype Constant a b = 
    Constant { getConstant :: a } 
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

instance Monoid a => 
        Applicative (Constant a) where
  pure x = Constant mempty
  (Constant f) <*> (Constant x) = Constant $ f <> x
