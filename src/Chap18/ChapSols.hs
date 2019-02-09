module Chap18.ChapSols where

import Control.Monad (join)
import Control.Applicative (liftA2)

-- 1
j18 :: Monad m => m (m a) -> m a
j18 = join

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap 

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4
a18 :: Monad m => m a -> m (a -> b) -> m b
a18 ma mf = (flip (<*>)) ma mf 

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = go xs f []
  where
    go [] _ acc = return acc
    go (x:xs) f acc = do
        el <- f x
        go xs f (acc ++ [el])

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
