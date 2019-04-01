module Chap9.Inter2 where

import Chap9.Inter1 (myWords)

-- Zipping 
-- 1
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = [] 
myZip _ [] = [] 
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

-- 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = [] 
myZipWith _ _ [] = [] 
myZipWith f (x:xs) (y:ys) = (x `f` y) : (myZipWith f xs ys)

-- 3
zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = myZipWith (,) xs ys

-- Filtering 
-- 1
mulsOfThree = [x | x <- [1..30], x `rem` 3 == 0]

-- 2
calc' = length mulsOfThree

-- 3
myFilter :: String -> [String]
myFilter xs = filter (\x -> x /= "a" && x /= "an") $ myWords xs
