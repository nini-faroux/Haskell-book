module Chap10.ChapSols where 

-- 1
stops = "pbtdkg"
vowels = "aeiou"

combs :: [Char] -> [Char] -> [(Char, Char, Char)]
combs stops vowels = go stops vowels stops []
  where
    go _ [] _ acc       = reverse acc
    go s1 vs [] acc     = go (drop 1 s1) vs stops acc
    go [] vs _ acc      = go stops (drop 1 vs) stops acc
    go xs vs (y:ys) acc = go xs vs ys ((head xs, head vs, y) : acc)

-- 2
combOneChar :: Char -> [Char] -> [Char] -> [(Char, Char, Char)]
combOneChar c stops vowels = go c vowels stops []
  where
    go _ [] _ acc      = reverse acc 
    go p vs [] acc     = go p (drop 1 vs) stops acc 
    go p vs (y:ys) acc = go p vs ys ((p, head vs, y) : acc)

-- rewrite functions using folds

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

and' :: [Bool] -> Bool 
and' = foldr (&&) True

and'' :: [Bool] -> Bool
and'' = foldr (\a b -> if a == False then False else b) True

-- 1
or' :: [Bool] -> Bool
or' [] = False 
or' (x:xs)
  | x == True = True 
  | otherwise = or' xs

or2 :: [Bool] -> Bool
or2 = foldr (\a b -> if a == True then True else b) False

orr :: Bool -> Bool -> Bool
orr x y
  | x = True 
  | otherwise = y

orrPf :: [Bool] -> Bool 
orrPf = foldr orr False

orPf2 :: [Bool] -> Bool 
orPf2 = foldr (||) False

-- 2
any' :: (a -> Bool) -> [a] -> Bool 
any' _ [] = False 
any' f (x:xs) 
  | f x       = True 
  | otherwise = any' f xs

any2 :: (a -> Bool) -> [a] -> Bool 
any2 f = foldr (\x y -> if f x then True else y) False 

any3 :: (a -> Bool) -> [a] -> Bool
any3 f = foldr (\x y -> f x || y) False 

-- 3
myElem' :: Eq a => a -> [a] -> Bool 
myElem' x = any (==x) 

myElem2 :: Eq a => a -> [a] -> Bool 
myElem2 x xs = foldr (\a b -> if a == x then True else b) False xs

-- 4
rev' :: [a] -> [a] 
rev' [] = [] 
rev' xs = last xs : rev' (init xs)

rev2 :: [a] -> [a] 
rev2 xs = foldr (\a b -> b ++ [a]) [] xs

-- 5 
mapp :: (a -> b) -> [a] -> [b]
mapp _ [] = [] 
mapp f (x:xs) = (f x) : mapp f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\a b -> f a : b) [] xs

-- 6
filt :: (a -> Bool) -> [a] -> [a] 
filt _ [] = [] 
filt f (x:xs) 
  | f x       = x : (filt f xs)
  | otherwise = filt f xs

filt2 :: (a -> Bool) -> [a] -> [a] 
filt2 f xs = foldr (\x y -> if (f x) then x : y else y) [] xs

-- 7 
squish1 :: [[a]] -> [a] 
squish1 [[]] = [] 
squish1 (x:xs) = x ++ squish1 xs

squish2 :: [[a]] -> [a] 
squish2 xxs = foldr (\a b -> a ++ b) [] xxs

-- 8
squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f xs = foldr (\x y -> (f x) ++ y) [] xs

-- 9 
squishAgain :: [[a]] -> [a]
squishAgain = squishMap1 id

-- 10
maxBy :: (a -> a -> Ordering) -> [a] -> a 
maxBy f xs = foldr (\x y -> if (f x y) == GT then x else y) (last xs) xs

maxBy2 :: (a -> a -> Ordering) -> [a] -> Maybe a 
maxBy2 _ []  = Nothing
maxBy2 _ [x] = Just x
maxBy2 f (x:y:xs) 
  | f x y == GT = Just x 
  | otherwise = maxBy2 f (y:xs)

-- 11
minBy :: (a -> a -> Ordering) -> [a] -> a 
minBy f xs = foldr (\x y -> if (f x y) == LT then x else y) (last xs) xs
