module PCPExamples.C09_Cancellation.Chan2 where 

import Control.Concurrent hiding (Chan)
import Control.Exception (mask_)

type Stream a = MVar (Item a) 
type Pointer a = MVar (Stream a)

data Item a = Item a (Stream a) 
data Chan a = Chan (Pointer a) (Pointer a)

-- async exception versions of the 
-- Chan api 

readChan :: Chan a -> IO a 
readChan (Chan readVar _) = 
  modifyMVar readVar $ \stream -> do 
    Item val tail <- readMVar stream 
    return (tail, val)

writeChan :: Chan a -> a -> IO () 
writeChan (Chan _ writeVar) val = do 
  newHole <- newEmptyMVar 
  mask_ $ do 
    oldHole <- takeMVar writeVar 
    putMVar oldHole (Item val newHole) 
    putMVar writeVar newHole 

newChan :: IO (Chan a)
newChan = do 
  hole     <- newEmptyMVar 
  readVar  <- newMVar hole 
  writeVar <- newMVar hole 
  return $ Chan readVar writeVar 
