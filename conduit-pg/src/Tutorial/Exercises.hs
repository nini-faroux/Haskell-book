module Tutorial.Exercises where

import Conduit

-- 1. EXERCISE Rewrite sinkGiven to not use do-notation. Hint: it’ll be easier to go Applicative

-- solution
sink :: Monad m => ConduitT Int o m (String, Int) 
sink = takeC 5 .| mapC show .| foldC >>= \x -> sumC >>= \y -> return (x, y)

-- given
sinkGiven :: Monad m => ConduitT Int o m (String, Int)
sinkGiven = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)

exOne :: IO ()
exOne = do
    let resG = runConduitPure $ yieldMany [1..10] .| sinkGiven
    let resS = runConduitPure $ yieldMany [1..10] .| sink
    print resG
    print resS

-- 2. EXERCISE Modify transGiven so that it does something different for the first 3, second 3, 
-- and final 3 values from upstream, and drops all other values.

-- solution 
trans :: Monad m => ConduitT Int Int m () 
trans = do
    takeC 3 .| mapC (+ 10) 
    takeC 3 .| mapC (* 5) 
    takeC 3 .| mapC (^ 2)
 
-- given
transGiven :: Monad m => ConduitT Int Int m ()
transGiven = do
    takeC 5 .| mapC (+ 1)
    mapC (* 2)

exTwo :: IO ()
exTwo = runConduit $ yieldMany [1..100] .| trans .| mapM_C print 

-- 3. EXERCISE Reimplement yieldMany for lists using the yield primitive and monadic composition.
-- solution
yieldMany' :: Monad m => [a] -> ConduitT i a m ()
yieldMany' xs = 
  if null xs
     then return () 
     else yield (head xs) >> yieldMany' (drop 1 xs)

testYieldSol :: IO () 
testYieldSol = runConduit $ yieldMany' "ab" .| mapM_C print

testYieldSol' :: IO ()
testYieldSol' = runConduit $ yieldMany' [1..100] .| trans .| mapM_C print

-- 4. EXERCISE Try implementing filterC and mapMC. For the latter, you’ll need to use the lift function.
filterC' :: Monad m => (a -> Bool) -> ConduitT a a m () 
filterC' f = loop
  where
    loop = do
        mx <- await 
        case mx of 
          Nothing -> return () 
          Just x -> if f x then yield x >> loop 
                           else loop 

testFilter :: IO () 
testFilter = runConduit $ yieldMany [1..10] 
                       .| filterC' even 
                       .| mapM_C print
