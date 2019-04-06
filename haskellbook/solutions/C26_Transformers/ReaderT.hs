{-# LANGUAGE InstanceSigs #-}

module C26_Transformers.ReaderT where

import Control.Monad.IO.Class 
import Control.Monad.Trans

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

instance MonadTrans (ReaderT r) where 
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

instance MonadIO m =>
        MonadIO (ReaderT r m) where 
  liftIO :: IO a -> ReaderT r m a
  liftIO = liftIO
