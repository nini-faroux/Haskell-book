module PCPExamples.C08_OverLappingIO.GetUrls1 (mainGetUrl1) where

import           PCPExamples.Helpers.Timeit
import           PCPExamples.Helpers.GetUrl
import           Control.Concurrent
import           Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B 

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a) 
async action = do 
  var <- newEmptyMVar 
  forkIO $ do 
    r <- action 
    putMVar var r 
  return $ Async var 

wait :: Async a -> IO a 
wait (Async var) = readMVar var 

timeDownload :: String -> IO () 
timeDownload url = do 
  (page, time) <- timeit $ getURL url 
  print (url, B.length page, time)

mainGetUrl1 :: IO () 
mainGetUrl1 = do 
  as <- mapM (async . timeDownload) sites 
  mapM_ wait as 
