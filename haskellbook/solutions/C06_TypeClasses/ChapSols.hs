module C06_TypeClasses.ChapSols where

import Data.List (sort)

{-- TypeKD --}
-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f _ x = f x

{-- does it typecheck --}
-- 1, fixed 
data Person = Person Bool deriving Show

printPerson :: Person -> IO () 
printPerson person = putStrLn (show person)

-- 2, fixed
data Mood = Blah'' | Woot'' deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Woot'' 
                  then Blah'' 
                  else x

{-- DT declaration & funcs, fixes --}
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1
phew = Papu (Rocks "chases") (Yeah True) 

-- 4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

instance Ord Papu where
  (>) (Papu (Rocks s) (Yeah b)) (Papu (Rocks s') (Yeah b')) = s > s' && b > b'
  (<) (Papu (Rocks s) (Yeah b)) (Papu (Rocks s') (Yeah b')) = s < s' && b < b'

{-- Match the types --}
-- 1
i' :: Num a => a
i' = 1

-- 2
f2' :: Float
f2' = 1.0

-- 3
-- Fractional instead of Float
f3' :: Fractional a => a 
f3' = 1.0

-- 4
-- Typechecks as Fractional is superclass of RealFrac
f4' :: RealFrac a => a
f4' = 1.0

-- 5
-- adding Ord typechecks, but pointless
freud :: Ord a => a -> a
freud x = x

-- 6
-- typechecks and constrains x 
freud' :: Int -> Int 
freud' x = x

-- 7, 8 
myX = 1 :: Int 

-- ignored argument can be redefined as 'a'
-- but result must be 'Int'
sigmund :: a -> Int
sigmund x = myX

-- 9
-- works with Int as it implements Ord
jung :: [Int] -> Int 
jung xs = head $ sort xs

-- 10
young :: Ord a => [a] -> a
young xs = head $ sort xs

-- 11
mySort :: [Char] -> [Char]
mySort = sort 

sig :: [Char] -> Char
sig xs = head $ mySort xs
