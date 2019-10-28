module PCPExamples.C08_OverLappingIO.GetUrls2 
        ( mainGetUrl2
        , mainGetUrl2'
        ) where

import           PCPExamples.Helpers.Timeit
import           PCPExamples.Helpers.GetUrl 
import qualified Data.ByteString as B
import           Control.Concurrent
import           Control.Exception

newtype Async a = Async (MVar (Either SomeException a)) 

async :: IO a -> IO (Async a) 
async action = do 
  var <- newEmptyMVar 
  forkIO $ do 
    r <- try action 
    putMVar var r 
  return $ Async var 

waitCatch :: Async a -> IO (Either SomeException a) 
waitCatch (Async var) = readMVar var 

wait :: Async a -> IO a 
wait a = do
  r <- waitCatch a
  case r of 
    Left e  -> throwIO e 
    Right a -> return a 

mainGetUrl2 :: IO () 
mainGetUrl2 = do 
  as <- mapM (async . timeDownload) sites 
  mapM_ wait as

mainGetUrl2' :: IO ()
mainGetUrl2' = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
