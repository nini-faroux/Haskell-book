module Tutorial.Examples.EvensOdds where 

import Conduit 

tagger :: Monad m => ConduitT Int (Either Int Int) m () 
tagger = mapC $ \i -> if even i then Left i else Right i 

evens, odds :: Monad m => ConduitT Int String m () 
evens = mapC $ \i -> "even num " ++ show i 
odds = mapC $ \i -> "Odd num " ++ show i 

left :: Either l r -> Maybe l 
left = either Just (const Nothing) 

right :: Either l r -> Maybe r 
right = either (const Nothing) Just 

inside :: Monad m => ConduitT (Either Int Int) String m () 
inside = getZipConduit $ ZipConduit (concatMapC left .| evens) *> ZipConduit (concatMapC right .| odds)

testEOs :: IO () 
testEOs = runConduit 
        $ enumFromToC 1 10 
       .| tagger 
       .| inside 
       .| mapM_C putStrLn
