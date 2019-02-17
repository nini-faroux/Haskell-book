{-# LANGUAGE InstanceSigs #-}

module Chap23.StateInstance where 

newtype State' s a = State' { runState' :: s -> (a, s) }

instance Functor (State' s) where 
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s -> (f . fst $ g s, snd $ g s)
