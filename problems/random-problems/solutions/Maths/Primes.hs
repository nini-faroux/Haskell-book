{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maths.Primes where 

import           Control.Monad.ST
import           Data.Array.ST (STUArray, runSTUArray, writeArray, readArray, newArray)
import           Control.Monad (when)
import           Data.Maybe (fromJust)
import           Data.Array.Unboxed (UArray)
import qualified Data.Vector as V 
import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S 
import           Data.Monoid ((<>))
import           Criterion.Main (defaultMain, bench, whnf) 

sieve :: Int -> [Int]
sieve n = go (ceiling . sqrt $ fromIntegral n) [2..(ceiling $ fromIntegral n)] 1
  where 
    go m xs idx 
      | idx >= m  = xs 
      | otherwise = go m (take idx xs <> filter (not . multiple (last $ take idx xs)) (drop idx xs)) (idx+1)

sieveS :: Int -> S.Seq Int 
sieveS n = go (ceiling . sqrt $ fromIntegral n) (S.fromList [2..(ceiling $ fromIntegral n)]) 1
  where
    go m xs idx 
      | idx >= m  = xs 
      | otherwise = go m (S.take idx xs S.>< S.filter (not . multiple (seqLast $ S.take idx xs)) (S.drop idx xs)) (idx+1)

seqLast :: S.Seq a -> a 
seqLast s = fromJust $ S.lookup (S.length s - 1) s

-- really slow, generating the vector causes a long pause
sieveV :: Int -> V.Vector Int 
sieveV n = go (ceiling . sqrt $ fromIntegral n) (V.fromList [2..ceiling $ fromIntegral n]) 1
  where 
    go m xs idx 
      | idx >= m  = xs 
      | otherwise = go m (V.take idx xs <> V.filter (not . multiple (V.last $ V.take idx xs)) (V.drop idx xs)) (idx+1)

multiple :: Int -> Int -> Bool 
multiple x y = y `mod` x == 0

sieveST :: forall s. Int -> ST s (STUArray s Int Bool)
sieveST n = do
    array <- newArray (0, n) True
    mapM_ (\i -> writeArray array i False) [0, 1]
    let m = ceiling . sqrt $ fromIntegral n

    let go :: Int -> ST s ()
        go idx 
          | idx > m   = return ()
          | otherwise = do
              b <- readArray array idx
              let setFalse :: Int -> ST s ()
                  setFalse j 
                    | j > n = return ()
                    | otherwise = writeArray array j False >> setFalse (j + idx)
              when b (setFalse (idx*idx))
              go (idx+1)
    go 2
    return array

runSieveST :: Int -> UArray Int Bool
runSieveST n = runSTUArray $ sieveST n

primesBench :: IO () 
primesBench = defaultMain 
   [ bench "10^3-List"      $ whnf sieve 1000 
   , bench "10^3-STUnboxed" $ whnf runSieveST 1000
   , bench "10^3-Vector"    $ whnf sieveV 1000
   , bench "10^4-List"      $ whnf sieve 10000
   , bench "10^4-STUnboxed" $ whnf runSieveST 10000
   , bench "10^4-Sequence"  $ whnf sieveS 10000
   , bench "10^4-Vector"    $ whnf sieveV 10000
   , bench "10^5-List"      $ whnf sieve 100000
   , bench "10^5-STUnboxed" $ whnf runSieveST 100000
   , bench "10^5-Sequence"  $ whnf sieveS 100000
   , bench "10^5-Vector"    $ whnf sieveV 100000
   , bench "10^6-List"      $ whnf sieve 1000000
   , bench "10^6-STUnboxed" $ whnf runSieveST 1000000
   , bench "10^6-Sequence"  $ whnf sieveS 1000000
   , bench "10^6-Vector"    $ whnf sieveV 1000000
   ]

mainPrimes :: IO () 
mainPrimes = do
   print $ sieve 100
   print $ sieveV 100
   print $ sieveS 100 
   print $ runSieveST 10
