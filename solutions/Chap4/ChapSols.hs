module Chap4.ChapSols where

-- mood swing 
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood 
changeMood Blah = Woot 
changeMood _ = Blah

-- 8
isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome x = x == reverse x

-- 9
myAbs :: Integer -> Integer 
myAbs x = if x > 0 then x else x * (-1)

-- 10
swappy :: (a, b) -> (c, d) -> ((b, d), (a, c))
swappy t1 t2 = (,) (snd t1, snd t2) (fst t1, fst t2)

-- correcting syntax 
-- 1
x = (+)
fcs xs = w `x` 1 
  where w = length xs

-- 2
fid :: a -> a
fid = \x -> x

-- 3
ffst :: (a, b) -> a
ffst (a, b) = a
