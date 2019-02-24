{-# LANGUAGE InstanceSigs #-}

module Chap26.StateT where

newtype StateT s m a = 
        StateT { runStateT :: s -> m (a, s) }

instance Applicative m => 
        Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s -> swap <$> sequenceA (s, f . fst <$> sma s)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
