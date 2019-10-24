module PCPExamples.Helpers.GetUrl where

import           Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

getURL :: String -> IO B.ByteString
getURL url = L.toStrict <$> simpleHttp url

sites = [ "http://www.google.com"
        , "http://www.bing.com"
        , "http://www.yahoo.com"
        , "http://www.wikipedia.com/wiki/Spade"
        , "http://www.wikipedia.com/wiki/Shovel"
        ]
