{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

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

runCont :: (forall r. (a -> r) -> r) -> a 
runCont f = f id

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b 
  fmap f conta = Cont $ \g -> g (f $ unCont conta id)
