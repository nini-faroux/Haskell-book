{-# LANGUAGE InstanceSigs #-}

module C22_Reader.ReaderApplicative where

newtype Reader' r a = Reader' { runReader' :: r -> a }

instance Functor (Reader' r) where 
  fmap :: (a -> b) -> Reader' r a -> Reader' r b
  fmap f (Reader' ra) = Reader' $ \r -> f $ ra r

instance Applicative (Reader' r) where
  pure :: a -> Reader' r a
  pure x = Reader' $ const x

  (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
  (Reader' rab) <*> (Reader' ra) = Reader' $ \r -> rab r $ ra r
