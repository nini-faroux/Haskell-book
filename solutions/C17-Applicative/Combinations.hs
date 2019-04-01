module Chap17.Combinations where

import Control.Applicative (liftA3) 

stops17 :: String
stops17= "pbtdkg" 

vowels17 :: String
vowels17 = "aeiou"

-- $ combos stops17 vowels17 stops17
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,) 
