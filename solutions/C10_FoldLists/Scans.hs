module C10_FoldLists.Scans where

-- 1 
fibs :: [Integer]
fibs = take 20 $ 1 : scanl (+) 1 fibs

-- 2
fibsHund :: [Integer]
fibsHund = [x | x <- fibs, x < 100]

-- 3 
factScan :: Int -> Int 
factScan n = head . drop n $ scanl' (*) 1 [1..n]

factList :: Int -> [Int] 
factList n = take n $ scanl (*) 1 [1..n]

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f z xs = 
    z : (case xs of 
            [] -> []
            (x:xs) -> scanl' f (f z x) xs)
