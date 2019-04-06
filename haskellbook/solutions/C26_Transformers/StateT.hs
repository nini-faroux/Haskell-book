{-# LANGUAGE InstanceSigs #-}

module C26_Transformers.StateT where

import Control.Monad.Trans

newtype StateT s m a = 
        StateT { runStateT :: s -> m (a, s) }

instance Applicative m => 
        Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s -> swap <$> sequenceA (s, f . fst <$> sma s)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

instance Monad m =>
        Applicative (StateT s m) where
  pure :: a -> StateT s m a 
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = 
          StateT $ \s -> do
              a  <- fst <$> sma s 
              f  <- fst <$> smab s 
              s' <- snd <$> sma s
              return (f a, s')

instance Monad m => 
        Monad (StateT s m) where 
  return = pure 

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f =
          StateT $ \s -> do
              a  <- fst <$> sma s 
              s' <- snd <$> sma s
              runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = 
      StateT $ \s -> do
         a <- m 
         return (a, s)

instance MonadIO m => 
        MonadIO (StateT s m) where 
  liftIO :: IO a -> StateT s m a
  liftIO = liftIO
