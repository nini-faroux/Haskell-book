module PCPExamples.C08_OverLappingIO.GetUrls3 
        ( mainGetUrl3
        ) where 
import           PCPExamples.Helpers.Timeit
import           PCPExamples.Helpers.GetUrl 
import qualified Data.ByteString as B
import Control.Concurrent 
import Control.Exception 

newtype Async a = Async (MVar (Either SomeException a))

waitEither :: Async a -> Async b -> IO (Either a b) 
waitEither a b = do 
  m <- newEmptyMVar 
  forkIO $ do
    r <- try $ fmap Left (wait a) 
    putMVar m r 
  forkIO $ do 
    r <- try $ fmap Right (wait b) 
    putMVar m r 
  wait $ Async m

waitAny :: [Async a] -> IO a 
waitAny as = do
  m <- newEmptyMVar
  let forkWait a = forkIO $ do 
          r <- try (wait a)
          putMVar m r 
  mapM_ forkWait as
  wait $ Async m
     
wait :: Async a -> IO a 
wait a = do
  r <- waitCatch a
  case r of 
    Left e  -> throwIO e 
    Right a -> return a 

waitCatch :: Async a -> IO (Either SomeException a) 
waitCatch (Async var) = readMVar var 

async :: IO a -> IO (Async a) 
async action = do 
  var <- newEmptyMVar 
  forkIO $ do 
    r <- try action 
    putMVar var r 
  return $ Async var 

mainGetUrl3 :: IO () 
mainGetUrl3 = do 
  let 
    download url = do 
        r <- getURL url 
        return (url, r) 

  as <- mapM (async . download) sites 
  
  (url, r) <- waitAny as 
  print (url, B.length r) 
  mapM_ wait as 
