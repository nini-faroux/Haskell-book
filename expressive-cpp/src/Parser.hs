{-# LANGUAGE OverloadedStrings #-}

module Parser where 

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as P 
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Combinator as PC
import qualified Data.ByteString as BS

parseAllRecords :: P.Parser [[BS.ByteString]]
parseAllRecords = AC.many' parseRecord >>= \initRecords -> parseLastRecord >>= \lastRecord -> return $ initRecords ++ [lastRecord]

parseRecord :: P.Parser [BS.ByteString] 
parseRecord = AC.manyTill parseField (AC.string "\n") 

parseField :: P.Parser BS.ByteString 
parseField = AC.takeTill (\c -> c == ',' || c == '\r') <* P.take 1

parseLastRecord :: P.Parser [BS.ByteString] 
parseLastRecord = AC.many' parseField >>= \initFields -> parseLastField >>= \lastField -> return $ initFields ++ [lastField]

parseLastField :: P.Parser BS.ByteString 
parseLastField = AC.takeTill (== ' ')
