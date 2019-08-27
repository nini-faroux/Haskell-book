{-# LANGUAGE InstanceSigs #-}

module LinkedLists.Stream where 

import Control.Comonad

infixr 5 :> 

data Stream a = a :> Stream a 
    deriving (Ord, Eq, Show)

instance Functor Stream where 
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where 
  pure = repeatS 

  (<*>) :: Stream (a -> b) -> Stream a -> Stream b 
  (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

instance Monad Stream where 
  return = pure 

  (>>=) :: Stream a -> (a -> Stream b) -> Stream b 
  xs >>= f = f (headS xs) >>= \y -> y :> (tailS xs >>= f)

instance Comonad Stream where 
  extract :: Stream a -> a 
  extract = headS 

  duplicate :: Stream a -> Stream (Stream a) 
  duplicate = pure 

  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend sf xs = sf (pure $ extract xs) :> extend sf (tailS xs)

repeatS :: a -> Stream a 
repeatS x = x :> repeatS x

headS :: Stream a -> a 
headS (x :> _) = x 

tailS :: Stream a -> Stream a 
tailS (_ :> xs) = xs 
