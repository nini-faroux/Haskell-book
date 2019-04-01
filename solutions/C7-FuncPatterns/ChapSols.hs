module Chap7.ChapSols where

-- 1 (a)
tensDigit :: Integral a => a -> a 
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- rewrite with divMod
tensDigit' :: Integral a => a -> a
tensDigit' x = snd $ divMod (x `div` 10) 10

-- 1 (c)
hundredsDigit :: Integral a => a -> a
hundredsDigit x = fst $ (snd $ x `divMod` 1000) `divMod` 100

-- 1 (c), messing 
hundredsDigit' :: (Integral a, Show a) => a -> Int
hundredsDigit' x = read (take 1 $ drop ((length $ show x) - 3) (show x)) :: Int

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

-- with case 
foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case b of False -> x
                            _     -> y
-- with guard 
foldBool'' :: a -> a -> Bool -> a
foldBool'' x y b 
  | b         = y
  | otherwise = x

-- 3 
g7 :: (a -> b) -> (a, c) -> (b, c)
g7 f t = (f $ fst t, snd t)

-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read $ show a

-- 5, pointfree
roundTrip' :: (Show a, Read a) => a -> a 
roundTrip' = read . show

-- 6
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

testRT :: IO () 
testRT = do
  print (roundTrip'' 4 :: Int)

-- 2, VP
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- lambdas
-- 3, a
addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of 
  True -> f n
  _    -> n
  where f = \n -> n + 1

-- b
addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y -> (if x > y then y else x) + 5

-- c
mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x
