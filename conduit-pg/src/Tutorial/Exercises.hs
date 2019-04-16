module Tutorial.Exercises where

import Conduit

-- EXERCISE Rewrite sinkGiven to not use do-notation. Hint: it’ll be easier to go Applicative

sink :: Monad m => ConduitT Int o m (String, Int) 
sink = undefined

sinkGiven :: Monad m => ConduitT Int o m (String, Int)
sinkGiven = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)

exOne :: IO ()
exOne = do
    let res = runConduitPure $ yieldMany [1..10] .| sink
    print res
