module Maths.Primes where 

import Data.Monoid ((<>))

sieve :: (RealFrac a, Floating a) => a -> [Int]
sieve n = go (ceiling $ sqrt n) [2..(ceiling n)] 1
  where 
    go n xs idx 
      | idx >= n = xs 
      | otherwise = go n (take idx xs <> filter (not . multiple (last $ take idx xs)) (drop idx xs)) (idx+1)

multiple :: Int -> Int -> Bool 
multiple x y = y `mod` x == 0
