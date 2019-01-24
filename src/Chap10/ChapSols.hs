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
-- 1
and' :: [Bool] -> Bool 
and' = foldr (&&) True

or' :: [Bool] -> Bool 
or' = foldr (||) False

-- 2
any' :: (a -> Bool) -> [a] -> Bool 
any' _ [] = False 
any' f (x:xs) 
  | f x       = True 
  | otherwise = any' f xs

any'' :: (a -> Bool) -> [a] -> Bool 
any'' f = foldr (\x y -> if f x then True else y) False 

any''' :: (a -> Bool) -> [a] -> Bool
any''' = undefined

-- 3
myElem' :: Eq a => a -> [a] -> Bool 
myElem' x = any (==x) 

myElem'' :: Eq a => a -> [a] -> Bool 
myElem'' = undefined
