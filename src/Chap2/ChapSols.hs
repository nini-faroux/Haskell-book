module Chap2.ChapSols where

piSquare :: Double -> Double
piSquare x = 3.14 * (x * x)

area :: Floating a => a -> a
area x = pi * (x * x)

-- rewrite 'let in' with 'where'
mult1 = x * y
   where x = 5
         y = 6

f = x * x + y 
   where x = 3 
         y = 1000

f1 = x * 5
  where x = y * 5 + y
        y = 10

f2 = z / x + y 
  where z = y * 10
        y = negate x 
        x = 7

waxOn = x * 5 
  where x = y ^ 2 
        y = z + 8 
        z = 7

triple x = x * 3

waxOff x = triple x
