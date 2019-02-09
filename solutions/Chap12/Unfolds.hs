module Chap12.Unfolds where

-- 1
myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : iterate f (f x)

-- 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = (fromJust' $ fst <$> f x) : (myUnfoldr f (fromJust' $ snd <$> f x))

fromJust' :: Maybe a -> a 
fromJust' (Just x) = x

-- 3
betterIterate :: (a -> a) -> a -> [a] 
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x
