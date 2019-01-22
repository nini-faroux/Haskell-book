module Chap9.Ciphers where

import Data.Char (chr, ord)

data Direction = Back | Forward deriving (Show, Eq)
type Steps = Int 

alphabet :: [(Char, Int)]
alphabet = zipWith (,) ['a'..'z'] (map ord ['a'..'z'])

caesar :: Steps -> Direction -> String -> String 
caesar step dir xs = go step dir xs [] 
  where 
    go step dir [] acc = reverse acc
    go step dir (x:xs) acc = go step dir xs ((conv step dir x) : acc) 

conv :: Steps -> Direction -> Char -> Char 
conv step dir c1 
     | dir == Forward = chr $ ((ord c1 - 97 + step) `rem` 26) + 97
     | otherwise      = chr $ ((ord c1 - 97 - step) `mod` 26) + 97
