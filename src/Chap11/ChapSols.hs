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
-- Split a sentence into words, then tuple each word with the capitalized form of each
-- capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = zipWith (,) (capWord <$> wordss xs) (wordss xs)
    where
      capWord s = (toUpper' $ head s) : (tail s)

wordss :: String -> [String]
wordss [] = []
wordss xs = (takeWhile (/= ' ') xs) : (wordss (drop 1 $ dropWhile (/= ' ') xs))

toUpper' :: Char -> Char 
toUpper' c 
  | (ord c <= 122 && ord c >= 97) = chr $ ord c - 32
  | otherwise = c

-- Language exercises 
-- 1
capitalizeWord :: String -> String
capitalizeWord s = [toUpper' $ head s] ++ tail s
