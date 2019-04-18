module Tutorial.Examples.ResourceAlloc where 

import Conduit 
import Data.ByteString (ByteString)
import qualified System.IO as IO 
import System.FilePath (takeExtension)
import qualified Data.Text as T 
import Data.Char (toUpper)

allocOne :: IO () 
allocOne = IO.withBinaryFile "test/input.txt" IO.ReadMode $ \inH -> 
           IO.withBinaryFile "test/output.txt" IO.WriteMode $ \outH -> 
           runConduit $ sourceHandle inH .| sinkHandle outH

allocTwo :: IO () 
allocTwo = withSourceFile "test/input.txt" $ \source -> 
           withSinkFile "test/output.txt" $ \sink -> 
           runConduit $ source .| sink

------- 
sourceFile' :: MonadResource m => FilePath -> ConduitT i ByteString m () 
sourceFile' fp = bracketP (IO.openBinaryFile fp IO.ReadMode) IO.hClose sourceHandle

sinkFile' :: MonadResource m => FilePath -> ConduitT ByteString o m () 
sinkFile' fp = bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose sinkHandle

allocThree :: IO () 
allocThree = runResourceT
           $ runConduit 
           $ sourceFile' "test/input.txt" 
          .| sinkFile' "test/output.txt"

--------
allocFour :: IO () 
allocFour = runConduitRes $ sourceFile "test/input.txt" .| sinkFile "test/output.txt"

--------
allocFive :: IO () 
allocFive = runConduitRes 
          $ sourceDirectoryDeep True "."
         .| filterC (\fp -> takeExtension fp == ".hs")
         .| awaitForever sourceFile 
         .| sinkFile "all-haskell-files"
