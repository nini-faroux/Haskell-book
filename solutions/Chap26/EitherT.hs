{-# LANGUAGE InstanceSigs #-} 

module Chap26.EitherT where

import Control.Monad.Trans

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

instance Monad m => 
        Monad (EitherT e m) where
  return = pure 

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = 
          EitherT $ do
              e <- ema 
              case e of 
                  (Left x)  -> return $ Left x
                  (Right y) -> runEitherT $ f y

instance MonadTrans (EitherT e) where 
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

-- 4 
swapEitherT :: Functor m => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEither :: Either e a -> Either a e 
swapEither (Left x) = Right x 
swapEither (Right y) = Left y

-- 5
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c 
eitherT f g (EitherT amb) = do 
        e <- amb
        case e of 
            (Left x)  -> f x 
            (Right y) -> g y
