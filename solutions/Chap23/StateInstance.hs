{-# LANGUAGE InstanceSigs #-}

module Chap23.StateInstance where 

newtype State' s a = State' { runState' :: s -> (a, s) }

instance Functor (State' s) where 
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s -> (f . fst $ g s, snd $ g s)

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure x = State' $ \s -> (x, s)

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  (State' sab) <*> (State' g) = State' $ \s -> ((fst $ sab s) (fst $ g s), snd (g s))

instance Monad (State' s) where
  return = pure 

  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = State' $ \s -> runState' (g . fst $ f s) (snd $ f s)

-- 1
get' :: State' s s
get' = State' $ \s -> (s, s)

-- 2
put' :: s -> State' s () 
put' s = State' $ \_ -> ((), s)

-- or
put'' :: s -> State' s () 
put'' s = State' $ const ((), s)

-- 3
exec :: State' s a -> s -> s 
exec (State' sa) s = snd $ runState' (State' sa) s

-- 4
eval' :: State' s a -> s -> a
eval' (State' sa) s = fst $ runState' (State' sa) s

-- 5
modify :: (s -> s) -> State' s () 
modify f = State' $ \s -> ((), f s)
