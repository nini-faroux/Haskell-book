module C16_Functor.HeavyLifting where

-- 1
a1 :: [Int]
a1 = (+1) <$> read "[1]" :: [Int]
-- or 
a2 :: [Int]
a2 = fmap (+1) (read "[1]" :: [Int])

-- 2
b1 :: Maybe [String]
b1 = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c1 :: Integer -> Integer 
c1 = (*2) . (\x -> x - 2)

-- 4
d1 :: Integer -> String 
d1 = ((return '1' ++) . show) . (\x -> [x, 1..3])

-- 5
e5 :: IO Integer
e5 = let ioi = readIO "1" :: IO Integer
         changed = read <$> (("123"++) . show <$> ioi) 
     in (*3) <$> changed
