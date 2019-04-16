module Tutorial.Folds where 

import Conduit 
import Data.Monoid (Product (..), Sum (..))

foldOne :: IO () 
foldOne = print $ runConduitPure $ yieldMany [1..100 :: Int] .| sumC

foldTwo :: IO () 
foldTwo = print $ runConduitPure $ yieldMany [1..100 :: Int] .| foldlC (+) 0

foldThree :: IO () 
foldThree = print $ getSum $ runConduitPure $ yieldMany [1..100 :: Int] .| foldMapC Sum

foldFour :: IO () 
foldFour = putStrLn $ runConduitPure 
         $ yieldMany [1..10 :: Int] 
        .| mapC (\i -> show i ++ "\n")
        .| foldC

foldFive :: IO () 
foldFive = putStrLn $ runConduitPure 
         $ yieldMany [1..10 :: Int] 
        .| mapC show 
        .| unlinesC 
        .| foldC 

foldSix :: IO () 
foldSix = print $ runConduitPure 
        $ yieldMany [1..10 :: Int] 
       .| foldlC (+) 0

foldSeven :: IO () 
foldSeven = print $ runConduitPure 
          $ yieldMany [1..10 :: Int] 
         .| foldMapC Sum

magic :: Int -> IO (Product Int) 
magic i = do
    putStrLn $ "magic on " ++ show i 
    return $ Product i 

foldEight :: IO () 
foldEight = do
    Product res <- runConduit $ yieldMany [1..10] .| foldMapMC magic 
    print res 
