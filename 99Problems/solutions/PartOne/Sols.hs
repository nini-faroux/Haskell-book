module PartOne.Sols where 

-- 1 Find the last element of a list
myLast :: [a] -> Maybe a 
myLast = foldr (\x xs -> if null xs then Just x else xs) Nothing 

-- 2 Find the last but one element of a list
sndLast :: [a] -> Maybe a
sndLast xs 
  | length xs < 2 = Nothing 
  | otherwise = Just . last $ init xs

-- unsafe, assuming list size > 1
sndLast' :: [a] -> a 
sndLast' = last . init
