module C05_Types.ChapSols where

-- write a type signature 
-- 1
fH :: [a] -> a
fH (x:_) = x

-- 2
fC :: Ord a => a -> a -> Bool  
fC x y = if (x > y) then True else False 

-- 3
fS :: (a, b) -> b
fS (_, y) = y

-- Given a type, write the function
myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ $ xToY x)

-- 1
ident :: a -> a 
ident = \x -> x

-- 2 
con :: a -> b -> a
con x _ = x

-- 3 
con' :: b -> a -> b
con' x _ = x

-- 4 
c'' :: a -> b -> b
c'' _ y = y

-- 5
r :: [a] -> [a]
r xs = xs ++ xs

rev :: [a] -> [a] 
rev [] = []
rev xs = last xs : rev (init xs)

-- 6 
co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f (g x)

-- 7 
a' :: (a -> c) -> a -> a
a' _ x = x

-- 8
a'' :: (a -> b) -> a -> b
a'' f = f 

-- Fix it 
-- 1
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if x > y then fstString x else sndString y
  where
    x = "Singin"
    y = "Somewhere"

-- 2
sing' :: [Char]
sing' = if x < y then fstString x else sndString y
  where 
    x = "Singin"
    y = "Somewhere"

-- 3
mainFix :: IO () 
mainFix = do
  print $ 1 + 2
  print 10
  print $ negate (-1)
  print ((+) 0 blah)
    where blah = negate 1
