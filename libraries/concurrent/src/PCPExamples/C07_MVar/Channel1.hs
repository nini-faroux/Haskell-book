module PCPExamples.C07_MVar.Channel1 where 

import Control.Concurrent

simpleChannel :: IO () 
simpleChannel = do 
  m <- newEmptyMVar 
  forkIO $ do
    putMVar m 'x' 
    putMVar m 'y' 
  r <- takeMVar m 
  print r 
  r <- takeMVar m 
  print r 

deadLock :: IO () 
deadLock = do 
  m <- newEmptyMVar 
  takeMVar m
