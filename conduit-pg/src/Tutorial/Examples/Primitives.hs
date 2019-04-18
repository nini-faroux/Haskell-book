{-# LANGUAGE ExtendedDefaultRules #-}

module Tutorial.Examples.Primitives where 

import Conduit

primOne :: IO () 
primOne = do 
  -- prints: Just 1
  print $ runConduitPure $ yield 1 .| await 

  -- prints Nothings 
  print $ runConduitPure $ yieldMany [] .| await 

  -- above is equivalent to following 
  print $ runConduitPure $ return () .| await
  print $ runConduitPure await

-- mapC using 'await' and 'yield'
myMapC :: Monad m => (i -> o) -> ConduitT i o m ()
myMapC f = loop 
  where
    loop = do
        mx <- await 
        case mx of 
          Nothing -> return () 
          Just x -> do 
            yield (f x) 
            loop

primTwo :: IO () 
primTwo = runConduit $ yieldMany [1..10] .| myMapC (+ 1) .| mapM_C print

primThree :: IO () 
primThree = print $ runConduitPure $ yieldMany [1..10] .| do
        x <- takeWhileC (<= 5) .| sinkList 
        y <- sinkList 
        return (x, y)

primFour :: IO () 
primFour = print $ runConduitPure $ return () .| do 
        mapM_ leftover [1..10]
        sinkList
