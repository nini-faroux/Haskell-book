module C09_Lists.ChapSols where 

import Data.Char

-- part 1
-- 2
uppers :: String -> String 
uppers s = filter isUpper s

-- 3
fstUpper :: String -> String
fstUpper [] = []
fstUpper s = (toUpper $ head s) : tail s

-- 4
caps :: String -> String 
caps [] = [] 
caps (x:xs) = (toUpper x) : caps xs

-- 5, 6
capHead :: String -> Char 
capHead = toUpper . head

-- last section 
-- 1
myOr :: [Bool] -> Bool 
myOr [] = False
myOr (x:xs) 
  | x = True 
  | otherwise = myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] = False
myAny f (x:xs)
  | f x = True 
  | otherwise = myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False
myElem a (x:xs)
  | a == x = True 
  | otherwise = myElem a xs

elem' :: Eq a => a -> [a] -> Bool
elem' a xs = any (==a) xs

-- 4
myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse xs = (last xs) : myReverse (init xs)

-- 5
squish :: [[a]] -> [a]
squish [] = [] 
squish (x:xs) = x `comb` squish xs

comb :: [a] -> [a] -> [a]
comb [] ys = ys
comb xs [] = xs 
comb (x:xs) ys = x : (comb xs ys)

-- 6 
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

-- 7 
squish' :: [[a]] -> [a] 
squish' = squishMap id

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ []  = Nothing
myMaximumBy _ [x] = Just x
myMaximumBy f (x:y:xs)
  | f x y == GT = Just x
  | otherwise   = myMaximumBy f (y:xs)

-- 9 
myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a 
myMinimumBy _ []  = Nothing 
myMinimumBy _ [x] = Just x
myMinimumBy f (x:y:xs) 
  | f x y == LT = Just x
  | otherwise   = myMinimumBy f (y:xs)

-- 10
myMax :: Ord a => [a] -> Maybe a
myMax = myMaximumBy compare

myMin :: Ord a => [a] -> Maybe a
myMin = myMinimumBy compare
