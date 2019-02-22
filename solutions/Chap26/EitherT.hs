{-# LANGUAGE InstanceSigs #-} 

module Chap26.EitherT where

newtype EitherT e m a = 
    EitherT { runEitherT :: m (Either e a) } 

instance Functor m => 
        Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => 
        Applicative (EitherT e m) where
  pure :: a -> EitherT e m a 
  pure x = EitherT . pure $ pure x

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emab) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema
