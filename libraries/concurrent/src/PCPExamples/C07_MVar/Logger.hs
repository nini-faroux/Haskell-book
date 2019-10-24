module PCPExamples.C07_MVar.Logger where 

import Control.Concurrent (MVar, takeMVar, putMVar, newEmptyMVar, forkIO)

newtype Logger = Logger (MVar LogCommand) 

data LogCommand = 
    Message String 
  | Stop (MVar ())

initLogger :: IO Logger 
initLogger = do 
  m <- newEmptyMVar 
  let l = Logger m 
  forkIO $ logger l 
  return l

logger :: Logger -> IO () 
logger (Logger m) = loop 
  where 
    loop = do 
      cmd <- takeMVar m 
      case cmd of 
        Message msg -> do 
          putStrLn msg 
          loop 
        Stop s -> do 
          putStrLn "logger: stop" 
          putMVar s () 

logMessage :: Logger -> String -> IO () 
logMessage (Logger m) s = putMVar m (Message s) 

logStop :: Logger -> IO () 
logStop (Logger m) = do 
  s <- newEmptyMVar 
  putMVar m (Stop s) 
  takeMVar s 

loggerMain :: IO () 
loggerMain = do 
  l <- initLogger 
  logMessage l "hello" 
  logMessage l "bye" 
  logStop l 
