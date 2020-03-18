{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (ix, (&), (.~))
import qualified Data.ByteString as BS 
import qualified Data.Attoparsec.ByteString as P
import Parser 

main :: IO ()
main = do 
  file <- BS.readFile "./data/expressive_cpp17_before.csv"
  columnName <- BS.getLine
  newValue <- BS.getLine
  if BS.null file 
    then print "Input file missing"
    else case P.parseOnly parseAllRecords file of 
            Left e -> print e 
            Right original ->
             if not $ columnExists columnName original 
                then putStrLn "Column name doesn’t exist in the input file"
                else do 
                  print original 
                  print $ replaceColumns columnName newValue original


replaceColumns :: BS.ByteString -> BS.ByteString -> [[BS.ByteString]] -> [[BS.ByteString]] 
replaceColumns columnName newValue rows = 
  head rows : replaceCols (getColumnNumber columnName (head rows)) newValue (drop 1 rows) 

replaceCols :: Int -> BS.ByteString -> [[BS.ByteString]] -> [[BS.ByteString]] 
replaceCols colNum newValue rows = overWrite colNum newValue <$> rows

overWrite :: Int -> BS.ByteString -> [BS.ByteString] -> [BS.ByteString] 
overWrite index newValue xs = xs & ix index .~ newValue

getColumnNumber :: BS.ByteString -> [BS.ByteString] -> Int 
getColumnNumber columnName row = go row 0 
  where 
    go [] acc = acc 
    go (c:cs) acc 
      | c == columnName = acc 
      | otherwise = go cs (acc + 1) 

columnExists :: BS.ByteString -> [[BS.ByteString]] -> Bool 
columnExists columnName rows = columnExists' columnName $ head rows

columnExists' :: BS.ByteString -> [BS.ByteString] -> Bool
columnExists' columnName headerRow = (not . null) $ filter (== columnName) headerRow
