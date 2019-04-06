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

-- 4
-- Find the number of elements of a list.
length' :: [a] -> Int 
length' = foldr (\_ y -> 1 + y) 0

-- 5
-- Reverse a list 
rev :: [a] -> [a] 
rev = foldr (\x xs -> xs ++ [x]) []

rev' :: [a] -> [a] 
rev' [] = [] 
rev' xs = last xs : rev (init xs)

