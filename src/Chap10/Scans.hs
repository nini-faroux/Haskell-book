module Chap10.Scans where

-- 1 
fibs :: [Integer]
fibs = take 20 $ 1 : scanl (+) 1 fibs

-- 2
fibsHund :: [Integer]
fibsHund = [x | x <- fibs, x < 100]

-- 3 
