module Tutorial.Examples.Fibs where 

import Conduit

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

indexedFibs :: ConduitT () (Int, Int) IO () 
indexedFibs = getZipSource 
            $ (,) 
           <$> ZipSource (yieldMany [1..]) 
           <*> ZipSource (yieldMany fibs) 

testFibs :: IO () 
testFibs = runConduit 
         $ indexedFibs 
        .| takeC 10
        .| mapM_C print 
