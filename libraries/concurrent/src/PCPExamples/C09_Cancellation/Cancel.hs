module PCPExamples.C09_Cancellation.Cancel (cancelMain1) where 

import Control.Concurrent 
import Control.Exception
import Control.Monad (when, forever)
import Data.Either (rights)
import System.IO
import PCPExamples.Helpers.Timeit 
import PCPExamples.Helpers.GetUrl

data Async a = Async ThreadId (MVar (Either SomeException a))

cancel :: Async a -> IO () 
cancel (Async tid _) = throwTo tid ThreadKilled 

waitCatch :: Async a -> IO (Either SomeException a) 
waitCatch (Async _ var) = readMVar var

async :: IO a -> IO (Async a) 
async action = do 
  var <- newEmptyMVar 
  t   <- forkIO $ do 
            a <- try action 
            putMVar var a 
  return $ Async t var

-- this is broken, if an exception happens 
-- between the first and second expression 
-- or the second and third expression 
-- the MVar will be left empty
problem :: MVar a -> (a -> IO a) -> IO () 
problem var f = do 
  a <- takeMVar var 
  r <- f a `catch` \e -> do 
                putMVar var a 
                throw (e :: SomeException) 
  putMVar var a 

-- fixed with 'mask' 
-- mask :: ((IO a -> IO a) -> IO b) -> IO b
problem' :: MVar a -> (a -> IO a) -> IO () 
problem' var f = mask $ \restore -> do 
  a <- takeMVar var 
  r <- restore (f a) `catch` \e -> do 
          putMVar var a 
          throw (e :: SomeException) 
  putMVar var r 

cancelMain1 :: IO () 
cancelMain1 = do 
  as <- mapM (async . timeDownload) sites 

  forkIO $ do 
      hSetBuffering stdin NoBuffering 
      forever $ do 
          c <- getChar 
          when (c == 'q') $ mapM_ cancel as 
  
  rs <- mapM waitCatch as 
  print (length $ rights rs, length rs) 
