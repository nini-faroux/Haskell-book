module Tutorial.Transformations where 

import Conduit 

transOne :: IO () 
transOne = runConduit 
         $ yieldMany [1..10]
        .| mapC (* 2) 
        .| mapM_C print

transTwo :: IO () 
transTwo = runConduit 
         $ yieldMany [1..10] 
        .| filterC even 
        .| mapM_C print 

transThree :: IO () 
transThree = runConduit
           $ yieldMany [1..100]
          .| filterC (\x -> odd x && x > 60)
          .| mapM_C print

transFour :: IO () 
transFour = runConduit 
          $ yieldMany [1..10]
         .| intersperseC 0 
         .| mapM_C print 

transFive :: IO () 
transFive = runConduit 
          $ yieldMany (map (replicate 5) [1..10])
         .| concatC 
         .| mapM_C print

evenM :: Int -> IO Bool
evenM i = do
    let res = even i 
    print (i, res) 
    return res 

transSix :: IO () 
transSix = runConduit 
         $ yieldMany [1..10] 
        .| filterMC evenM 
        .| mapM_C print 

transSeven :: IO () 
transSeven = do
    res <- runConduit $ yieldMany [1..10] .| iterMC print .| sumC 
    print res
