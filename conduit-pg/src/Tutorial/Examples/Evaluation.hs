module Tutorial.Examples.Evaluation where

import Conduit 

evalOne :: IO () 
evalOne = runConduit 
        $ yieldMany [1..10] 
       .| iterMC print 
       .| return () 
       
evalTwo :: IO () 
evalTwo = runConduit 
        $ yieldMany [1..10]
       .| iterMC print 
       .| sinkNull

evalThree :: IO () 
evalThree = runConduit 
          $ yieldMany [1..10]
         .| iterMC print 
         .| liftIO (putStrLn "yep")
         .| sinkNull
