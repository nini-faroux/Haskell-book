{-# LANGUAGE InstanceSigs #-}

module Chap26.MaybeT where 

import Control.Monad.IO.Class
import Control.Monad.Trans

newtype MaybeT m a = 
    MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m =>
        Functor (MaybeT m) where 
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance Applicative m => 
        Applicative (MaybeT m) where 
  pure :: a -> MaybeT m a 
  pure x = MaybeT . pure $ pure x

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ (<*>) <$> mab <*> ma 

instance Monad m => 
        Monad (MaybeT m) where 
  return = pure 

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b 
  (MaybeT m) >>= f = 
          MaybeT $ do
             may <- m 
             case may of 
                   Nothing  -> return Nothing
                   (Just x) -> runMaybeT $ f x

instance MonadTrans MaybeT where 
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeT $ Just <$> m

instance (MonadIO m) =>
        MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = liftIO
