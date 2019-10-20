module Stm.Counter where 

import Control.Concurrent.STM 
import Control.Monad (replicateM_, forever)

type Counter = TVar Int

makeCounter :: IO Counter
makeCounter = newTVarIO 1

increment :: Counter -> IO ()
increment var = atomically $ do
  val <- readTVar var 
  writeTVar var $! val + 1

incrementAndGet :: Counter -> IO Int 
incrementAndGet c = do 
  oldVar <- readTVarIO c 
  increment c
  return oldVar

printCounter :: IO Counter -> IO () 
printCounter c = do 
  val  <- c >>= \c' -> readTVarIO c'
  print val 

mainCounter :: IO () 
mainCounter = do 
  counter <- makeCounter 
  replicateM_ 10 $ incrementAndGet counter >>= print 
