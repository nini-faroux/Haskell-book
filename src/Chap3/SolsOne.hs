module Chap3.SolsOne where

-- Chapter exercises 
-- 2
appendExcl :: String -> String 
appendExcl s = s ++ "!"

-- takeDrop 4 "Curry is awesome!" -> "y"
takeDrop :: Int -> String -> String
takeDrop n s = [head $ drop n s]

-- 3
thirdLetter :: String -> Char 
thirdLetter s = head $ drop 2 s

-- 4
letterIndex :: Int -> Char 
letterIndex x = head $ drop x "curry is awesome"

-- 5
-- "Curry is awesome" -> "awesome is Curry"
rvrs :: String -> String 
rvrs s = drop 9 s ++ (take 4 $ drop 5 s) ++ take 5 s

-- 6
mainThree :: IO ()
mainThree = print $ rvrs "Curry is awesome"
