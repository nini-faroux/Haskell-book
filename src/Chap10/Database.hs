module Chap10.Database where 

import Data.Time

data DatabaseItem = 
    DbString String 
  | DbNumber Integer
  | DbDate UTCTime 
  deriving (Eq, Show, Ord)

theDatabase :: [DatabaseItem]
theDatabase = 
        [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbString "Hello, world!"
        , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
        , DbDate (UTCTime (fromGregorian 1942 3 5) (secondsToDiffTime 34123))
        ]

-- 1
filterDate :: [DatabaseItem] -> [UTCTime]
filterDate db = getDate <$> filter isDate db

-- 2
filterNums :: [DatabaseItem] -> [Integer]
filterNums db = getNum <$> filter isNum db

-- 3
mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent [] = Nothing
mostRecent db = go (filterDate db) (head $ filterDate db)
  where 
    go [] acc = Just acc
    go (x:xs) acc 
      | x > acc   = go xs x
      | otherwise = go xs acc

-- 4
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterNums

-- 5
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral . sum $ filterNums db) / (fromIntegral . length $ filterNums db)

------

isDate :: DatabaseItem -> Bool 
isDate (DbDate _) = True
isDate _ = False

isNum :: DatabaseItem -> Bool
isNum (DbNumber _) = True
isNum _ = False

getNum :: DatabaseItem -> Integer
getNum (DbNumber n) = n 

getDate :: DatabaseItem -> UTCTime
getDate (DbDate date) = date 
