module PCPExamples.C07_MVar.UnboundedChannel where 

import Control.Concurrent.MVar

type Stream a = MVar (Item a) 
type Pointer a = MVar (Stream a)

data Item a = Item a (Stream a) 
data Chan a = Chan (Pointer a) (Pointer a)

newChan :: IO (Chan a)
newChan = do 
  hole     <- newEmptyMVar 
  readVar  <- newMVar hole 
  writeVar <- newMVar hole 
  return $ Chan readVar writeVar 

writeChan :: Chan a -> a -> IO () 
writeChan (Chan _ writeVar) val = do 
  oldHole <- takeMVar writeVar 
  newHole <- newEmptyMVar 
  putMVar oldHole (Item val newHole) 
  putMVar writeVar newHole 

readChan :: Chan a -> IO a 
readChan (Chan readVar _) = do 
  stream <- takeMVar readVar 
  (Item val next) <- readMVar stream 
  putMVar readVar next 
  return val
  
dupChan :: Chan a -> IO (Chan a) 
dupChan (Chan _ writeVar) = do 
  hole <- readMVar writeVar 
  newReadVar <- newMVar hole 
  return $ Chan newReadVar writeVar

-- can lead to deadlock if the
-- channel is empty
unGetChan :: Chan a -> a -> IO () 
unGetChan (Chan readVar _) val = do  
  newRead <- newEmptyMVar 
  oldRead <- takeMVar readVar 
  putMVar newRead (Item val oldRead) 
  putMVar readVar newRead 
