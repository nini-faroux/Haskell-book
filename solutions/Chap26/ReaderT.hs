{-# LANGUAGE InstanceSigs #-}

module Chap26.ReaderT where

newtype ReaderT r m a = 
        ReaderT { runReaderT :: r -> m a }

instance Functor m =>
        Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m =>
        Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure x = ReaderT . pure $ pure x 

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma 

instance Monad m =>
        Monad (ReaderT r m) where
  return = pure 

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= \x -> runReaderT (f x) r
