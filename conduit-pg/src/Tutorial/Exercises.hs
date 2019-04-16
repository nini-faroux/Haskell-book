module Tutorial.Exercises where

import Conduit

-- EXERCISE Rewrite sinkGiven to not use do-notation. Hint: it’ll be easier to go Applicative

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
