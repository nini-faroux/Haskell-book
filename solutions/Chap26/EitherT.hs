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

-- 4 
swapEitherT :: Functor m => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

swapEither :: Either e a -> Either a e 
swapEither (Left x) = Right x 
swapEither (Right y) = Left y