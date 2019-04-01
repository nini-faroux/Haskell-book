{-# LANGUAGE InstanceSigs #-}

module Chap22.ReaderMonad where

import Control.Monad (join)

newtype Reader r a = Reader { runReader :: r -> a } 

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f $ ra r

instance Applicative (Reader r) where 
  pure :: a -> Reader r a
  pure x = Reader $ const x

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
  return = pure 

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r

  -- with join
  --(Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)
