module PCPExamples.Helpers.Timeit where 

import           Data.Time
import qualified Data.ByteString as B 
import           PCPExamples.Helpers.GetUrl 

timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

timeDownload :: String -> IO () 
timeDownload url = do 
  (page, time) <- timeit $ getURL url 
  print (url, B.length page, time)

