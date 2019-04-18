module Tutorial.Examples.Composition where

import Conduit 

source :: Monad m => ConduitT i Int m () 
source = do
    yieldMany [1..10] 
    yieldMany [11..20]

source2 :: Monad m => ConduitT i Int m () 
source2 = do
    yieldMany [11..20]
    yieldMany [1..10]

compOne :: IO () 
compOne = runConduit $ source .| mapM_C print

compTwo :: IO () 
compTwo = runConduit $ source2 .| mapM_C print

--- 

sink :: Monad m => ConduitT Int o m (String, Int) 
sink = do
    x <- takeC 5 .| mapC show .| foldC 
    y <- sumC 
    return (x, y)

compThree :: IO () 
compThree = do
    let res = runConduitPure $ yieldMany [1..10] .| sink 
    print res
