module Tutorial.Examples.Chunked where 

import Conduit
import qualified Data.Text as T 
import Data.Char (toUpper)

chunkOne :: IO () 
chunkOne = runConduitRes
         $ sourceFile "test/input.txt"
        .| decodeUtf8C 
        .| mapC (T.map toUpper)
        .| encodeUtf8C
        .| stdoutC

chunkTwo :: IO () 
chunkTwo = runConduitRes 
           $ sourceFile "test/input.txt"
          .| decodeUtf8C
          .| omapCE toUpper 
          .| encodeUtf8C 
          .| stdoutC 

chunkThree :: IO () 
chunkThree = runConduitRes 
           $ sourceFile "test/input.txt"
          .| decodeUtf8C 
          .| takeWhileCE (/= '\n')
          .| encodeUtf8C 
          .| stdoutC

chunkFour :: IO () 
chunkFour = runConduitRes 
          $ sourceFile "test/input.txt"
         .| takeCE 5
         .| stdoutC
