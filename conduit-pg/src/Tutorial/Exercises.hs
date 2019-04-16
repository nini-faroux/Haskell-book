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
exTwo = runConduit $ yieldMany [1..10] .| trans .| mapM_C print 
