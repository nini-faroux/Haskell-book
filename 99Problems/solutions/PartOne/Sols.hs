module PartOne.Sols where 

-- 1 
-- Find the last element of a list
myLast :: [a] -> Maybe a 
myLast = foldr (\x xs -> if null xs then Just x else xs) Nothing 

-- 2 
-- Find the last but one element of a list
sndLast :: [a] -> Maybe a
sndLast xs 
  | length xs < 2 = Nothing 
  | otherwise = Just . last $ init xs

-- unsafe, assuming list size > 1
sndLast' :: [a] -> a 
sndLast' = last . init

-- 3 
-- Find the K'th element of a list. The first element in the list is number 1
kthElem :: [a] -> Int -> Maybe a 
kthElem xs n
  | n > length xs || n < 1 = Nothing 
  | otherwise              = Just . head $ drop (n-1) xs
