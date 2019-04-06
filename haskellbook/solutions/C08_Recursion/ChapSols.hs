module C08_Recursion.ChapSols where

import Data.Maybe (fromJust)
import Data.List (intersperse)

-- recursion 
-- 2
sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll n = go n 1 0
   where 
     go n count acc 
       | count == n = acc + n 
       | otherwise = go n (count+1) (acc+count)

-- 3
mul :: Integral a => a -> a -> a
mul x y = go x y 0 0
   where 
     go x y count acc
       | count == y = acc 
       | otherwise  = go x y (count + 1) (acc + x)

-- dividedBy
data DividedResult =
        Result Integer 
      | DividedByZero
      deriving (Eq, Show)

divide :: Integral a => a -> a -> DividedResult 
divide _ 0 = DividedByZero
divide x y = go x y 0 
    where
      go x y acc 
        | x < y = Result acc 
        | otherwise = go (x-y) y (acc+1)

-- McCarthy 91
mc :: Integral a => a -> a
mc n 
  | n > 100 = n - 10
  | otherwise = mc . mc $ n + 11

-- Numbers to words 
nums :: [(Int, String)]
nums = [(0, "zero"), (1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]

digitToWord :: Int -> String 
digitToWord n = fromJust $ lookup n nums

digits :: Int -> [Int] 
digits n = go n [] 
  where 
    go n acc
      | (fst $ n `divMod` 10) == 0 = (snd $ n `divMod` 10) : acc
      | otherwise = go (fst $ n `divMod` 10) ((snd $ n `divMod` 10) : acc)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" $ digitToWord <$> digits n

---------
-- digits using string instead
digits' :: Int -> [Int]
digits' n = go (show n) 
  where 
    go :: String -> [Int]
    go [] = [] 
    go (x:xs) = (fromJust $ lookup x digs) : go xs 

digs :: [(Char, Int)]
digs = zipWith (,) ['0'..'9'] [0..9]
