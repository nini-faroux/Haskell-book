module Chap22.ReaderPractise where

import Control.Applicative 
import Data.Maybe

xrp = [1,2,3]
yrp = [4,5,6]
zrp = [7,8,9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' a = foldr (\x y -> if fst x == a then Just (snd x) else y) Nothing 

xs' :: Maybe Integer 
xs' = lookup' 3 $ zip xrp yrp

ys' :: Maybe Integer 
ys' = lookup' 6 $ zip yrp zrp 

zs' :: Maybe Integer 
zs' = lookup' 4 $ zip xrp yrp

z' :: Integer -> Maybe Integer 
z' n = lookup' n $ zip xrp zrp

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs' ys'

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys' zs'

x3' :: Integer -> (Maybe Integer, Maybe Integer) 
x3' n = (,) (z' n) (z' n)

---------------------------- 

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

summed' :: Num c => (c, c) -> c 
summed' = uncurry' (+)

bolt :: Integer -> Bool 
bolt = liftA2 (&&) (>3) (<8)

fromPerhaps :: a -> Maybe a -> a 
fromPerhaps x Nothing = x 
fromPerhaps _ (Just x) = x

seqA :: Integral a => a -> [Bool]
seqA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed' <$> ((,) <$> xs' <*> ys')

boolFold :: Integer -> [Bool]
boolFold n = (foldr (&&) True) <$> (seqA <$> [1..n])

seqS :: [Bool]
seqS = fromPerhaps [] $ seqA <$> s'

boltYs :: Bool
boltYs = fromPerhaps False $ bolt <$> ys'

mainRead :: IO () 
mainRead = do
    print boltYs
    print seqS
    print $ boolFold 10
