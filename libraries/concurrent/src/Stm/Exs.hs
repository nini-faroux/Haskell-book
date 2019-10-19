module Stm.Exs where 

import Control.Concurrent.STM 
import Control.Monad (replicateM_, forever)

makeCounter :: IO (IO Int)
makeCounter = do 
  var <- newTVarIO 1 
  return undefined 

stmOne :: IO () 
stmOne = do 
  counter <- makeCounter 
  replicateM_ 10 $ counter >>= print 


