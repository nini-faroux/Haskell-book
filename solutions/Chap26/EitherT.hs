{-# LANGUAGE InstanceSigs #-} 

module Chap26.EitherT where

newtype EitherT e m a = 
    EitherT { runEitherT :: m (Either e a) } 

instance Functor m => 
        Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema
