{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- 
  Exercise 6.3-i
  What is the rank of:  
  'Int -> forall a. a -> a' 

  Rank-1, as there 1 arrow after the deepest forall 

  Ex 6.3-ii 
  What is the rank of:
  '(a -> b) -> (forall c. c -> a) -> b' 

  Rank-2, 2 arrows after deepest forall

  Ex 6.3-iii 
  What is the rank of: 
  '((forall x. mx -> b (z m x) -> b (z m a)) -> m a'

  Rank-3, 3 arrows after deepest forall
-}

{-
  Exercise 6.4-i 
  Provide a Functor, Applicative and Monad 
  instances for Cont 
-}

newtype Cont a = 
  Cont { 
    unCont :: forall r. (a -> r) -> r 
  }

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b 
  fmap f (Cont ca) = Cont $ \g -> ca (g . f)

  -- first effort, worked... 
  -- fmap f ca = Cont $ \g -> g (f $ unCont ca id)

instance Applicative Cont where 
  pure :: a -> Cont a 
  pure x = Cont $ \f -> f x 

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (Cont f) <*> Cont a = 
      Cont $ \br -> f $ \ab -> a $ br . ab 

-- first effort, worked  
-- cab <*> ca = Cont $ \g -> g $ (unCont cab id) (unCont ca id)

instance Monad Cont where 
  return = pure 

  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  ca >>= aCb = Cont $ \c -> unCont ca $ \a -> unCont (aCb a) c

--  first effort, type checks, works with 'releaseStringCont'
--  ca >>= f = f $ unCont ca id

----------------------------------------------

runCont :: (forall r. (a -> r) -> r) -> a 
runCont f = f id

withVersionNumber :: (Double -> r) -> r 
withVersionNumber f = f 1.0

withTimeStamp :: (Int -> r) -> r 
withTimeStamp f = f 1532083362

withOS :: (String -> r) -> r 
withOS f = f "linux" 

releaseStringCont :: String 
releaseStringCont = runCont $ unCont $ do
    version <- Cont withVersionNumber 
    date    <- Cont withTimeStamp 
    os      <- Cont withOS
    pure $ os ++ "-" ++ show version ++ "-" ++ show date
