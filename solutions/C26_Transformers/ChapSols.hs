module C26_Transformers.ChapSols where 

import C22_Reader.ReaderMonad
import C26_Transformers.ReaderT
import Control.Monad.Identity
import Control.Monad.Trans.State.Lazy

-- 1
rDec :: Num a => Reader a a 
rDec = Reader dec

dec :: Num a => a -> a 
dec x = x - 1

-- 2
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (return . show)

-- 3
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT (\r -> putStrLn ("Hi: " ++ show r) >> return (inc r))

inc :: Num a => a -> a 
inc x = x + 1

-- 4
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\s -> return (show s, inc s))
