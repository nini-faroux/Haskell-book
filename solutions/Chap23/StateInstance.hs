{-# LANGUAGE InstanceSigs #-}

module Chap23.StateInstance where 

newtype State' s a = State' { runState' :: s -> (a, s) }

instance Functor (State' s) where 
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s -> (f . fst $ g s, snd $ g s)

instance Semigroup s =>
        Applicative (State' s) where
  pure :: a -> State' s a
  pure x = State' $ \s -> (x, s)

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  (State' sab) <*> (State' g) = State' $ \s -> ((fst $ sab s) (fst $ g s), snd (sab s) <> snd (g s))

instance Semigroup s =>
        Monad (State' s) where
  return = pure 

  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = State' $ \s -> runState' (g . fst $ f s) (snd (f s) <> s)
