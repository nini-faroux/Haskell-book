module Tutorial.Examples.CalcHash where 

import Conduit 
import Crypto.Hash.Conduit (sinkHash) 
import Crypto.Hash (Digest, SHA256) 
import Network.HTTP.Simple (httpSink, parseRequest)

calcHash :: IO ()
calcHash = do
    req <- parseRequest "http://httpbin.org" 
    digest <- runResourceT 
            $ httpSink req (\_res -> getZipSink (ZipSink (sinkFile "test/hash.txt") *> ZipSink sinkHash))
    print (digest :: Digest SHA256)
