module Chap11.ChapSols where

import Data.Char (chr, ord)

-- 1
-- return True if all values in first list appear in the second list
isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] _ = True 
isSubseqOf (x:xs) ys
  | filter (==x) ys == [] = False 
  | otherwise             = isSubseqOf xs ys

-- 2
-- capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = zipWith (,) (capWord <$> wordss xs) (wordss xs)

wordss :: String -> [String]
wordss [] = []
wordss xs = (takeWhile (/= ' ') xs) : (wordss (drop 1 $ dropWhile (/= ' ') xs))

toUpper' :: Char -> Char 
toUpper' c 
  | (ord c <= 122 && ord c >= 97) = chr $ ord c - 32
  | otherwise = c

-- Language exercises 
-- 1
capWord :: String -> String
capWord s = [toUpper' $ head s] ++ tail s

-- 2
capPar :: String -> String 
capPar [] = [] 
capPar xs = go (capWord xs) ' ' []
  where
    go [] prev acc = reverse acc
    go (x:xs) prev acc 
      | prev == '.' && x /= ' ' = go xs x ((toUpper' x) : acc)
      | prev == '.' && x == ' ' = go xs prev (x : acc)
      | otherwise = go xs x (x : acc)
