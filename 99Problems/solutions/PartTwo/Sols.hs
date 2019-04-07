module PartTwo.Sols where 

import PartOne.Sols

-- 11
-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

data Encode a =
    Single a 
  | Multiple Int a
  deriving Show 

encodeMod :: Eq a => [a] -> [Encode a] 
encodeMod xs = enc <$> pack xs

enc :: [a] -> Encode a 
enc xs 
  | length xs > 1 = Multiple (length xs) (head xs)
  | otherwise     = Single (head xs)
