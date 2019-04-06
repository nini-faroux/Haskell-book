{-# LANGUAGE InstanceSigs #-} 

module C22_Reader.Ask where

newtype Reader' r a = Reader' { runReader' :: r -> a } 

instance Functor (Reader' r) where
  fmap :: (a -> b) -> Reader' r a -> Reader' r b
  fmap f (Reader' ra) = Reader' $ \r -> f (ra r)

ask' :: Reader' a a 
ask' = Reader' $ \x -> x

ask'' :: Reader' a a
ask'' = Reader' id

asks' :: (r -> a) -> Reader' r a 
asks' f = Reader' $ \x -> f x

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

liftATwo :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftATwo = ((<*>) .) . (<$>)
